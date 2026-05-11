package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

/** Transforms calls to `scala.util.control.Breaks.break()` and `Breaks.breakable { body }`
  * inside an async block. `break()` is rewritten to `BreaksAsyncShift.break(Breaks)` so the
  * thrown `BreakControl` is wrapped as a `NonFatal`-friendly `ControlThrowableAsyncWrapper`,
  * letting the surrounding `breakable` shift catch it through the monad. The `breakable`
  * body is also passed through `substituteNonFatal` so user-level `case NonFatal(_)` catches
  * inside the body do not accidentally swallow the wrapper.
  */
trait BreaksTreeTransform[F[_], CT, CC <: CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  // Breaks.break()
  def runBreaksBreak(applyTerm: Apply)(owner: Symbol): CpsTree = {
    val shiftedCall =
      Apply(
        Select(shiftedBreaksObjectRef, shiftedBreaksBreakSym),
        List(Ref.term(breaksModuleSym.companionModule.termRef))
      )
    CpsTree.pure(owner, shiftedCall, true)
  }

  // Breaks.breakable { body }
  def runBreaksBreakable(applyTerm: Apply, fun: Term, args: List[Term])(owner: Symbol): CpsTree = {
    if (cpsCtx.flags.debugLevel >= 10) {
      cpsCtx.log("runBreaksBreakable")
    }
    val paramsDescriptor = MethodParamsDescriptor(fun)
    val substituteNonFatal = new TreeMap {
      override def transformTree(tree: Tree)(owner: Symbol): Tree = {
        tree match
          case u @ Unapply(fun, implicits, patterns) if fun.symbol == nonFatalUnapplySym =>
            val nFun = Select.unique(nonFatalAndNotControlThrowableAsyncWrapperCompanion, "unapply")
            Unapply.copy(u)(nFun, implicits, patterns)
          case _ =>
            super.transformTree(tree)(owner)
      }
    }
    val nArgs = substituteNonFatal.transformTerms(args)(owner)
    val argRecords = O.buildApplyArgsRecords(paramsDescriptor, nArgs)(owner)
    // The single argument is the by-name `op: => Unit`.
    argRecords.head match
      case appRecord: ApplyArgByNameRecord =>
        appRecord.cpsTree.syncOrigin match
          case None =>
            // Async body: build () => F[Unit] via the record's identArg with CPS_ONLY.
            val shiftedArg = appRecord.shift().identArg(true)
            val nTerm = Apply.copy(applyTerm)(
              Apply(
                TypeApply(
                  Select(shiftedBreaksObjectRef, shiftedBreaksBreakableSym),
                  List(TypeTree.of[F])
                ),
                List(Ref.term(breaksModuleSym.companionModule.termRef), cpsCtx.monad.asTerm)
              ),
              List(shiftedArg)
            )
            CpsTree.impure(owner, nTerm, applyTerm.tpe)
          case Some(syncBody) =>
            if (appRecord.cpsTree.isChanged) then
              // Build a () => Unit lambda over the transformed sync body, route through syncBreakable
              // so a wrapped break (from a rewritten Breaks.break()) is still recognised.
              val mt = MethodType(List())(_ => List(), _ => TypeRepr.of[Unit])
              val lambda = Lambda(owner, mt, (lamOwner, _) => syncBody.changeOwner(lamOwner))
              val nTerm = Apply.copy(applyTerm)(
                Apply(
                  Select(shiftedBreaksObjectRef, shiftedBreaksSyncBreakableSym),
                  List(Ref.term(breaksModuleSym.companionModule.termRef))
                ),
                List(lambda)
              )
              CpsTree.pure(owner, nTerm, true)
            else CpsTree.pure(owner, applyTerm)
      case _ =>
        throw MacroError("Invalid argument for Breaks.breakable (should be by-name)", posExpr(applyTerm))
  }
