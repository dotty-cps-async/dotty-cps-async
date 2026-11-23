package cps.plugin.forest

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Spans.Span

import cps.plugin.*
import cps.plugin.forest.application.*

/** A sync qualifier of an assignment `q.f = ...` or a method call `q.m(...)` with an async right part or argument is bound to a val
  * before the async part, to keep the Scala evaluation order, unless it is pure (see `isPure`) or not a value. See
  * https://github.com/dotty-cps-async/dotty-cps-async/issues/165 and https://github.com/dotty-cps-async/dotty-cps-async/issues/169
  *
  * The same holds for a sync argument of a call with an async argument (`f(a, await(...))`, see `ApplyArg.withSyncBinding` and
  * https://github.com/dotty-cps-async/dotty-cps-async/issues/170).
  */
object QualifierBinding {

  /** An async argument, which is evaluated before the call. An async by-name argument is evaluated by the called method: the
    * qualifier is not bound for it (as in the macro, where a by-name argument is not async).
    */
  def hasAsyncArgBeforeCall(argss: List[ApplyArgList])(using Context, CpsTopLevelContext): Boolean =
    argss.exists {
      case ApplyTermArgList(_, args) =>
        args.exists {
          case _: ByNameApplyArg => false
          case arg               => arg.isAsync
        }
      case _: ApplyTypeArgList => false
    }

  def needsBinding(qual: Tree)(using Context): Boolean =
    !isPure(qual) && !isNoValue(qual)

  /** Evaluated after the async part, also more than once (a monad can run the rest of the computation more than once, as `List`),
    * it gives the same value without other effects: a pure path (dotty's `isPurePath`), a function value, or the `StringContext`
    * of a string interpolation with pure parts, which must stay a literal for `f`. Differences from dotty's `exprPurity` (3.9):
    *   - a package is a pure path (`refPurity` gives an idempotent path for it, so `_root_.scala.StringContext` would be bound);
    *   - a member imported from a path (`import O.*; h`) is selected from it: the path is evaluated too;
    *   - a new instance, also of a case class without initialization code, is not pure: each evaluation gives a new one;
    *   - `StringContext(...)` can have varargs (`exprPurity` has no case for a `SeqLiteral`).
    * As in dotty, a selection from a val is pure also when the val can be null (as in the macro, see `QualifierPurity.PurePath`).
    */
  private def isPure(tree: Tree)(using Context): Boolean =
    tree match
      case _: Literal | _: This | _: Super    => true
      case _ if tree.symbol.is(Flags.Package) => true
      case id: Ident                          => isPureRef(tree) && importedQualifier(id).forall(isPure)
      case Select(qual, _)                    => isPureRef(tree) && isPure(qual)
      case Typed(expr, _)                     => isPure(expr)
      case Apply(fn, args)                    => fn.symbol == defn.StringContextModule_apply && isPure(fn) && args.forall(isPureArg)
      case closureDef(_)                      => true
      case _                                  => false

  private def isPureArg(arg: Tree)(using Context): Boolean =
    arg match
      case Typed(SeqLiteral(elems, _), _) => elems.forall(isPure)
      case SeqLiteral(elems, _)           => elems.forall(isPure)
      case NamedArg(_, expr)              => isPureArg(expr)
      case _                              => isPure(arg)

  /** `val name = qual`. A singleton type of a stable path, which is the value of `qual`, is kept: member types and the types of the
    * next parameters can depend on it. `qual` is such a path (a lazy val or an object), or has its type (`f()`, where `def f():
    * b.type`), or refers to a parameterless method, a by-name parameter or a var with such a declared type (`def get: b.type`).
    * Otherwise only singleton term references are widened (as `widenTermRefByName` in the macro): `this.type` stays.
    */
  def valDef(qual: Tree, name: String, span: Span, owner: Symbol)(using Context): ValDef = {
    def stablePathType(tpe: Types.Type): Option[Types.Type] =
      tpe match
        case tr: Types.TermRef if isPathType(tr) =>
          if (isIdempotentPath(ref(tr))) then Some(tr) else stablePathType(tr.info.widenExpr)
        case _ => None
    val qualType = stablePathType(qual.tpe).getOrElse(qual.tpe.widenTermRefExpr)
    val qualSym = newSymbol(owner, name.toTermName, Flags.EmptyFlags, qualType, NoSymbol)
    ValDef(qualSym.asTerm, qual.changeOwner(owner, qualSym)).withSpan(span)
  }

  /** `{ val q = qual; next }`, where `next` uses `q`.
    *
    * The block is built as one tree: changing the owner of a block tree (when the whole expression is moved, e.g. it is the right
    * part of an async `val`) also changes the owner of its local definitions. A `MemberDefCpsTree` in a `SeqCpsTree` would keep the
    * owner of the val: `changeOwner` of a ValDef tree does not change the owner of the ValDef's own symbol. An async lambda stays a
    * `CpsTree` (it is applied or shifted later), so this case still uses a `SeqCpsTree`; a call with a bound qualifier is not known
    * to give an async lambda.
    */
  def withBinding(origin: Tree, owner: Symbol, qualValDef: ValDef, next: CpsTree)(using Context, CpsTopLevelContext): CpsTree =
    next.asyncKind match
      case AsyncKind.Sync =>
        val nextTree = next.unpure.get.changeOwner(next.owner, owner)
        CpsTree.pure(origin, owner, Block(List(qualValDef), nextTree).withSpan(origin.span))
      case AsyncKind.Async(internalKind) =>
        val nextTree = next.transformed.changeOwner(next.owner, owner)
        CpsTree.impure(origin, owner, Block(List(qualValDef), nextTree).withSpan(origin.span), internalKind)
      case AsyncKind.AsyncLambda(_) =>
        // the lambda is kept as a CpsTree, to be applied or shifted
        BlockBoundsCpsTree(SeqCpsTree(origin, owner, IndexedSeq(MemberDefCpsTree(origin, owner, qualValDef)), next))

  /** The term path of a member imported from it (`import O.*; n = ...` or `import O.*; m(...)`), which is the qualifier of the
    * reference.
    */
  def importedQualifier(id: Ident)(using Context): Option[Tree] =
    id.tpe match
      case Types.TermRef(prefix: Types.TermRef, _) => Some(ref(prefix).withSpan(id.span))
      case _                                       => None

  /** The statics of a Java class, a package and a Scala.js global scope object are not values: never bound, no initialization
    * order.
    */
  private def isNoValue(tree: Tree)(using Context): Boolean = {
    val sym = tree.symbol
    // the annotation is compared by name, as in the macro: no class lookup for each qualifier
    sym.is(Flags.Package) || sym.isAllOf(Flags.JavaModule) || sym.is(Flags.Module) &&
      sym.moduleClass.annotations.exists(_.symbol.fullName.toString == "scala.scalajs.js.annotation.JSGlobalScope")
  }

  /** A reference can be built only to a path: `track().arr` has a TermRef type with a prefix that is not a path. */
  private def isPathType(tpe: Types.Type)(using Context): Boolean =
    tpe match
      case tr: Types.TermRef =>
        tr.prefix match
          case Types.NoPrefix | _: Types.ThisType => true
          case prefix                             => isPathType(prefix)
      case _ => false

}
