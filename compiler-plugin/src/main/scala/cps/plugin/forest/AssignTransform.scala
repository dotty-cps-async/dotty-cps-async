package cps.plugin.forest

import cps.plugin.*
import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*

object AssignTransform {

  def apply(term: Assign, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    lhsQualifier(term.lhs) match
      case Some(qualifier) =>
        val lhsQual = RootTransform(qualifier, owner, nesting + 1)
        lhsQual.asyncKind match
          case AsyncKind.Sync =>
            // the qualifier can be changed (e.g. contains an unshifted lambda), but stay sync
            val qual = lhsQual.unpure.get
            val cpsRhs = RootTransform(term.rhs, owner, nesting)
            cpsRhs.asyncKind match
              case AsyncKind.Async(_) if !isPurePath(qual) && !isNoValue(qual) =>
                // the qualifier is evaluated before the right part: `{ val q = qual; rhs.map(x => q.f = x) }`.
                // A singleton type of a stable path (a lazy val or an object, or the result type of `def f(): b.type`) is kept: the type
                // of the field can depend on it. Otherwise only singleton term references are widened (as `widenTermRefByName` in the
                // macro): `this.type` stays.
                val qualType = qual.tpe match
                  case tr: Types.TermRef if isIdempotentPath(ref(tr)) => tr
                  case _                                              => qual.tpe.widenTermRefExpr
                val qualSym = newSymbol(owner, "xQualAssign".toTermName, Flags.EmptyFlags, qualType, NoSymbol)
                val qualValDef = ValDef(qualSym.asTerm, qual.changeOwner(owner, qualSym)).withSpan(qualifier.span)
                val lhs = ref(qualSym).select(term.lhs.symbol).withSpan(term.lhs.span)
                BlockBoundsCpsTree(
                  SeqCpsTree(term, owner, IndexedSeq(MemberDefCpsTree(term, owner, qualValDef)), lhsSyncAssign(term, lhs, cpsRhs, owner))
                )
              case _ =>
                val lhs = if (qual eq qualifier) then term.lhs else qual.select(term.lhs.symbol).withSpan(term.lhs.span)
                lhsSyncAssign(term, lhs, cpsRhs, owner)
          case AsyncKind.Async(internalKind) =>
            lhsSelectAsyncAssign(term, term.lhs.symbol, lhsQual, owner, nesting)
          case AsyncKind.AsyncLambda(body) =>
            throw CpsTransformException(s"Can't assign sync value to async lambda", term.srcPos)
      case None =>
        val cpsLhs = RootTransform(term.lhs, owner, nesting)
        cpsLhs.asyncKind match
          case AsyncKind.Sync =>
            lhsSyncAssign(term, term.lhs, RootTransform(term.rhs, owner, nesting), owner)
          case AsyncKind.Async(internalKind) =>
            throw CpsTransformException(s"Impossible: async assignmebt which is not select, $cpsLhs", term.srcPos)
          case AsyncKind.AsyncLambda(body) =>
            throw CpsTransformException(s"Can't assign value to async lambda", term.srcPos)
  }

  /** The qualifier of the left part: of a selection, or the term path of an imported member (`import O.*; n = ...`). */
  private def lhsQualifier(lhs: Tree)(using Context): Option[Tree] =
    lhs match
      case sel: Select => Some(sel.qualifier)
      case id: Ident =>
        id.tpe match
          case Types.TermRef(prefix: Types.TermRef, _) => Some(ref(prefix).withSpan(id.span))
          case _                                       => None
      case _ => None

  /** The statics of a Java class, a package and a Scala.js global scope object are not values: never bound, no initialization order. */
  private def isNoValue(tree: Tree)(using Context): Boolean = {
    val sym = tree.symbol
    sym.is(Flags.Package) || sym.isAllOf(Flags.JavaModule) || sym.is(Flags.Module) && {
      val jsGlobalScope = Symbols.getClassIfDefined("scala.scalajs.js.annotation.JSGlobalScope")
      jsGlobalScope.exists && sym.moduleClass.hasAnnotation(jsGlobalScope)
    }
  }

  def lhsSyncAssign(term: Assign, lhs: Tree, cpsRhs: CpsTree, owner: Symbol)(using Context, CpsTopLevelContext): CpsTree = {
    cpsRhs.asyncKind match
      case AsyncKind.Sync =>
        val rhs = cpsRhs.unpure.get
        if ((rhs eq term.rhs) && (lhs eq term.lhs)) then CpsTree.unchangedPure(term, owner)
        else CpsTree.pure(term, owner, Assign(lhs, rhs).withSpan(term.span))
      case AsyncKind.Async(internalKind) =>
        val sym = newSymbol(owner, "xAssign".toTermName, Flags.EmptyFlags, term.rhs.tpe.widen, Symbols.NoSymbol)
        val valDef = ValDef(sym.asTerm, EmptyTree)
        val nAssign = Assign(lhs, ref(sym)).withSpan(term.span)
        MapCpsTree(term, owner, cpsRhs, MapCpsTreeArgument(Some(valDef), CpsTree.unchangedPure(nAssign, owner)))
      case AsyncKind.AsyncLambda(body) =>
        throw CpsTransformException(s"Can't assign async lambda", term.srcPos)
  }

  def lhsSelectAsyncAssign(term: Assign, lhsSym: Symbol, lhsQual: CpsTree, owner: Symbol, nesting: Int)(using
      Context,
      CpsTopLevelContext
  ): CpsTree = {
    val qualSym = newSymbol(owner, "xQualAssign".toTermName, Flags.EmptyFlags, lhsQual.originType, NoSymbol)
    val qualValDef = ValDef(qualSym.asTerm, EmptyTree)
    val cpsRhs = RootTransform(term.rhs, owner, nesting)
    cpsRhs.asyncKind match
      case AsyncKind.Sync =>
        val nAssign = Assign(
          ref(qualSym).select(lhsSym).withSpan(term.lhs.span),
          cpsRhs.unpure.get
        ).withSpan(term.span)
        MapCpsTree(term, owner, lhsQual, MapCpsTreeArgument(Some(qualValDef), CpsTree.pure(term, owner, nAssign)))
      case AsyncKind.Async(internalKind) =>
        val rhsSym = newSymbol(owner, "xAssign".toTermName, Flags.EmptyFlags, cpsRhs.originType.widen, NoSymbol)
        val rhsValDef = ValDef(rhsSym, EmptyTree)
        val nAssign = Assign(ref(qualSym).select(lhsSym).withSpan(term.lhs.span), ref(rhsSym)).withSpan(term.span)
        FlatMapCpsTree(
          term,
          owner,
          lhsQual,
          FlatMapCpsTreeArgument(
            Some(qualValDef),
            MapCpsTree(term, owner, cpsRhs, MapCpsTreeArgument(Some(rhsValDef), CpsTree.unchangedPure(nAssign, owner)))
          )
        )
      case AsyncKind.AsyncLambda(bodyKind) =>
        throw CpsTransformException(s"Can't assign async lambda", term.srcPos)
  }

}
