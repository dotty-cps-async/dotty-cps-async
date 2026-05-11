package cps.plugin.forest

import cps.plugin.{AsyncKind, CpsTopLevelContext, CpsTransformHelper, TransformUtil}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*

object BreaksBreakableTransform {

  /** Transforms `Breaks.breakable { body }` so a `Breaks.break()` rewritten by
    * `BreaksBreakTransform` (which throws a `ControlThrowableAsyncWrapper`)
    * is caught and converted to a successful empty result.
    *
    * Async body  → `BreaksAsyncShift.breakable[F](Breaks, monad)(() => transformedBody)`
    * Sync body   → `BreaksAsyncShift.syncBreakable(Breaks)(() => body)` —
    *                runs the body inside a try/catch that unwraps a stray wrapper
    *                and delegates to stdlib `Breaks.breakable` for identity matching.
    *
    * The body is also passed through [[NonFatalSubstitution]] so that a user
    * `case NonFatal(_)` catch inside the body does not swallow the wrapper.
    */
  def apply(term: Apply, owner: Symbol, nesting: Int, arg: Tree)(using Context, CpsTopLevelContext): CpsTree = {
    val breaksAsyncShift = Symbols.requiredModule("cps.runtime.util.control.BreaksAsyncShift")
    val breaksModule = Symbols.requiredModule("scala.util.control.Breaks")

    val nArg = NonFatalSubstitution(arg)
    val cpsBody = RootTransform(nArg, owner, nesting + 1)
    cpsBody.unpure match
      case Some(syncBody) =>
        val nLambda = TransformUtil.makeLambda(Nil, defn.UnitType, owner, syncBody, owner)
        val nFun = Apply(
          Select(ref(breaksAsyncShift), "syncBreakable".toTermName).withSpan(term.fun.span),
          List(ref(breaksModule))
        ).withSpan(term.fun.span)
        val nApply = Apply(nFun, List(nLambda)).withSpan(term.span)
        CpsTree.pure(term, owner, nApply)
      case None =>
        val monadType = summon[CpsTopLevelContext].monadType
        val nBody = cpsBody.transformed
        val resultType = CpsTransformHelper.cpsTransformedType(defn.UnitType, monadType)
        val nLambda = TransformUtil.makeLambda(Nil, resultType, owner, nBody, owner)
        val nFun = Apply(
          TypeApply(
            Select(ref(breaksAsyncShift), "breakable".toTermName),
            List(TypeTree(monadType))
          ).withSpan(term.fun.span),
          List(ref(breaksModule), summon[CpsTopLevelContext].cpsMonadRef)
        ).withSpan(term.fun.span)
        val nApply = Apply(nFun, List(nLambda)).withSpan(term.span)
        CpsTree.impure(term, owner, nApply, AsyncKind.Sync)
  }

}
