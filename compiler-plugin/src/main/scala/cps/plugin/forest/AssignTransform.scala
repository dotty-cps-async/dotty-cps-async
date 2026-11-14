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
    term.lhs match
      case sel: Select =>
        val lhsQual = RootTransform(sel.qualifier, owner, nesting + 1)
        lhsQual.asyncKind match
          case AsyncKind.Sync =>
            // the qualifier can be changed (e.g. contains an unshifted lambda), but stay sync
            val qual = lhsQual.unpure.get
            val cpsRhs = RootTransform(term.rhs, owner, nesting)
            cpsRhs.asyncKind match
              case AsyncKind.Async(_) if !isStablePath(qual, allowLazy = false) =>
                // the qualifier is evaluated before the right part: `{ val q = qual; rhs.map(x => q.f = x) }`.
                // A stable path through a lazy val keeps its singleton type: the type of the field can depend on it.
                val qualType = if (isStablePath(qual, allowLazy = true)) then qual.tpe else qual.tpe.widen
                val qualSym = newSymbol(owner, "xQualAssign".toTermName, Flags.EmptyFlags, qualType, NoSymbol)
                val qualValDef = ValDef(qualSym.asTerm, qual.changeOwner(owner, qualSym))
                val lhs = ref(qualSym).select(sel.symbol).withSpan(sel.span)
                BlockBoundsCpsTree(
                  SeqCpsTree(term, owner, IndexedSeq(MemberDefCpsTree(term, owner, qualValDef)), lhsSyncAssign(term, lhs, cpsRhs, owner))
                )
              case _ =>
                val lhs = if (qual eq sel.qualifier) then term.lhs else qual.select(sel.symbol).withSpan(sel.span)
                lhsSyncAssign(term, lhs, cpsRhs, owner)
          case AsyncKind.Async(internalKind) =>
            lhsSelectAsyncAssign(term, sel, lhsQual, owner, nesting)
          case AsyncKind.AsyncLambda(body) =>
            throw CpsTransformException(s"Can't assign sync value to async lambda", term.srcPos)
      case _ =>
        val cpsLhs = RootTransform(term.lhs, owner, nesting)
        cpsLhs.asyncKind match
          case AsyncKind.Sync =>
            lhsSyncAssign(term, term.lhs, RootTransform(term.rhs, owner, nesting), owner)
          case AsyncKind.Async(internalKind) =>
            throw CpsTransformException(s"Impossible: async assignmebt which is not select, $cpsLhs", term.srcPos)
          case AsyncKind.AsyncLambda(body) =>
            throw CpsTransformException(s"Can't assign value to async lambda", term.srcPos)
  }

  /** `this`, `super`, or a selection of a non-lazy val or an object from a stable path: evaluating it after the right part gives the same
    * value without other effects. (A var, a lazy val, a by-name parameter or a method call is evaluated before the right part.) With
    * `allowLazy`, lazy vals are accepted too: a stable path, but its evaluation can initialize a lazy val.
    */
  private def isStablePath(tree: Tree, allowLazy: Boolean)(using Context): Boolean = {
    def isStableSymbol(sym: Symbol): Boolean =
      sym.isStableMember && (allowLazy || !sym.is(Flags.Lazy) || sym.is(Flags.Module))
    tree match
      case This(_) | Super(_, _) => true
      case Ident(_)              => isStableSymbol(tree.symbol)
      case Select(qual, _)       => isStableSymbol(tree.symbol) && isStablePath(qual, allowLazy)
      case _                     => false
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

  def lhsSelectAsyncAssign(term: Assign, lhsSelect: Select, lhsQual: CpsTree, owner: Symbol, nesting: Int)(using
      Context,
      CpsTopLevelContext
  ): CpsTree = {
    val qualSym = newSymbol(owner, "xQualAssign".toTermName, Flags.EmptyFlags, lhsQual.originType, NoSymbol)
    val qualValDef = ValDef(qualSym.asTerm, EmptyTree)
    val cpsRhs = RootTransform(term.rhs, owner, nesting)
    cpsRhs.asyncKind match
      case AsyncKind.Sync =>
        val nAssign = Assign(
          ref(qualSym).select(lhsSelect.symbol).withSpan(term.lhs.span),
          cpsRhs.unpure.get
        ).withSpan(term.span)
        MapCpsTree(term, owner, lhsQual, MapCpsTreeArgument(Some(qualValDef), CpsTree.pure(term, owner, nAssign)))
      case AsyncKind.Async(internalKind) =>
        val rhsSym = newSymbol(owner, "xAssign".toTermName, Flags.EmptyFlags, cpsRhs.originType.widen, NoSymbol)
        val rhsValDef = ValDef(rhsSym, EmptyTree)
        val nAssign = Assign(ref(qualSym).select(lhsSelect.symbol).withSpan(term.lhs.span), ref(rhsSym)).withSpan(term.span)
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
