package cps.macros.common

import scala.quoted.*

trait PatternAwareTreeMapScope:

  thisScope: MacroReflectScope =>

  import qctx.reflect.*

  /** TreeMap which parses the pattern of a `CaseDef` top-down, passing to `transformTerm` and `transformTypeTree` only terms
    * (extractors, implicits, literal and stable-identifier patterns) and type trees. Macro TreeMaps which traverse user code should
    * extend it.
    *
    * The default TreeMap dispatches subpatterns via `transformTree`, where the named pattern `case C(name = p)`, typed as
    * `NamedArg(name, p)` inside `Unapply`, is handled as a term, and `transformTerm` fails on `p` with MatchError (see
    * tests-cli/t2026_09_24_named_pattern_treemap). Also the default TreeMap does not go inside the pattern of `Bind`.
    *
    * It is defined inside a scope, because a top-level `class PatternAwareTreeMap[Q <: Quotes & Singleton](using val q: Q) extends
    * q.reflect.TreeMap` is unusable: inherited methods are typed with `PatternAwareTreeMap.this.q.reflect.Term`, which does not
    * unify with `quotes.reflect.Term` at the call site.
    */
  abstract class PatternAwareTreeMap extends TreeMap:

    override def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef =
      CaseDef.copy(tree)(
        transformPattern(tree.pattern)(owner),
        tree.guard.map(transformTerm(_)(owner)),
        transformTerm(tree.rhs)(owner)
      )

    /** Transform the pattern, keeping the original trees when nothing in them is changed.
      *
      * `Bind.copy` receives the name as a String, so a compiler-generated (derived) name, as `$1$` in the desugaring of
      * `val (a, b) = e` into `e match { case $1$ @ Tuple2(a @ _, b @ _) => $1$ }`, becomes a simple name, and `-Wunused:patvars`
      * reports it as an unused pattern variable (https://github.com/dotty-cps-async/dotty-cps-async/issues/148).
      */
    def transformPattern(pattern: Tree)(owner: Symbol): Tree =
      pattern match
        case b: Bind =>
          val nPattern = transformPattern(b.pattern)(owner)
          if (nPattern eq b.pattern) b else Bind.copy(b)(b.name, nPattern)
        case u: Unapply =>
          transformUnapply(u)(owner)
        case a: Alternatives =>
          val nPatterns = a.patterns.map(transformPattern(_)(owner))
          if (allSame(nPatterns, a.patterns)) a else Alternatives.copy(a)(nPatterns)
        case na: NamedArg =>
          // value of NamedArg is a pattern, not a term.
          val nValue = transformPattern(na.value)(owner)
          if (nValue eq na.value) na else NamedArg.copy(na)(na.name, nValue.asInstanceOf[Term])
        case TypedOrTest(inner, tpt) =>
          val nInner = transformPattern(inner)(owner)
          val nTpt = transformTypeTree(tpt)(owner)
          if ((nInner eq inner) && isSameTree(nTpt, tpt)) pattern else TypedOrTest.copy(pattern)(nInner, nTpt)
        case w: Wildcard =>
          w
        case t: Term =>
          val nt = transformTerm(t)(owner)
          if (isSameTree(nt, t)) t else nt
        case other =>
          transformTree(other)(owner)

    /** Transform the extractor pattern. Can be overridden to substitute the extractor. */
    def transformUnapply(u: Unapply)(owner: Symbol): Unapply =
      val nFun = transformTerm(u.fun)(owner)
      val nImplicits = transformSubTrees(u.implicits)(owner)
      val nPatterns = u.patterns.map(transformPattern(_)(owner))
      if (isSameTree(nFun, u.fun) && nImplicits.corresponds(u.implicits)(isSameTree) && allSame(nPatterns, u.patterns)) u
      else Unapply.copy(u)(nFun, nImplicits, nPatterns)

    private def allSame(transformed: List[Tree], original: List[Tree]): Boolean =
      transformed.corresponds(original)(_ eq _)

    /** The default TreeMap creates new trees also when nothing is changed (for example, for `TypeApply`), so compare the
      * structure, referenced symbols and types.
      */
    private def isSameTree(transformed: Tree, original: Tree): Boolean =
      (transformed eq original) || (
        transformed.show(using Printer.TreeStructure) == original.show(using Printer.TreeStructure) &&
          treeAtoms(transformed).corresponds(treeAtoms(original)) { case ((s1, t1), (s2, t2)) => s1 == s2 && t1 =:= t2 }
      )

    /** symbols and types of references and type trees, in the order of traversal */
    private def treeAtoms(tree: Tree): List[(Symbol, TypeRepr)] =
      new TreeAccumulator[List[(Symbol, TypeRepr)]] {
        def foldTree(acc: List[(Symbol, TypeRepr)], tree: Tree)(owner: Symbol): List[(Symbol, TypeRepr)] =
          tree match
            case t: Ident    => (t.symbol, t.tpe) :: acc
            case t: Select   => foldTree((t.symbol, t.tpe) :: acc, t.qualifier)(owner)
            case t: This     => (t.symbol, t.tpe) :: acc
            case t: TypeTree => (t.symbol, t.tpe) :: acc
            case _           => foldOverTree(acc, tree)(owner)
      }.foldTree(Nil, tree)(Symbol.spliceOwner)
