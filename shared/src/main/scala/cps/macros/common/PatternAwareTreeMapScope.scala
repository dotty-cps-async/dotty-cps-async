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
    * Overrides of `transformTerm` and `transformTypeTree` should return the original tree when nothing in it is changed, as the
    * default TreeMap does (lists are transformed with `mapConserve`, and `copy` returns the original tree when the subtrees are the
    * same): `transformPattern` checks with `eq` whether the subtrees are changed, and rebuilds the pattern if they are.
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

    /** The default TreeMap does not transform the subtrees of `TypeBind`, `TypeSelect` and `TypeProjection`, but copies them with
      * the name as a String, which makes a new tree, and a derived name (as `T$1` of the type argument in `case Node[T](...)`)
      * becomes a simple name.
      */
    override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
      tree match
        case _: TypeBind | _: TypeSelect | _: TypeProjection => tree
        case _                                             => super.transformTypeTree(tree)(owner)

    /** Transform the pattern, keeping the original trees when nothing in them is changed.
      *
      * `Bind.copy` receives the name as a String, so a compiler-generated (derived) name, as `$1$` in the desugaring of
      * `val (a, b) = e` into `e match { case $1$ @ Tuple2(a @ _, b @ _) => $1$ }`, becomes a simple name, and `-Wunused:patvars`
      * reports it as an unused pattern variable (https://github.com/dotty-cps-async/dotty-cps-async/issues/148).
      * When the pattern of such a bind is changed, the bind is built from its symbol, which keeps the name, but receives the macro
      * position (binds with derived names are not reported by -Wunused). Binds with user names are copied, keeping the position.
      */
    def transformPattern(pattern: Tree)(owner: Symbol): Tree =
      pattern match
        case b: Bind =>
          val nPattern = transformPattern(b.pattern)(owner)
          if (nPattern eq b.pattern) b
          else if (b.name.contains('$')) Bind(b.symbol, nPattern)
          else Bind.copy(b)(b.name, nPattern)
        case u: Unapply =>
          transformUnapply(u)(owner)
        case a: Alternatives =>
          val nPatterns = a.patterns.mapConserve(transformPattern(_)(owner))
          if (nPatterns eq a.patterns) a else Alternatives.copy(a)(nPatterns)
        case na: NamedArg =>
          // value of NamedArg is a pattern, not a term.
          val nValue = transformPattern(na.value)(owner)
          if (nValue eq na.value) na else NamedArg.copy(na)(na.name, nValue.asInstanceOf[Term])
        case TypedOrTest(inner, tpt) =>
          val nInner = transformPattern(inner)(owner)
          val nTpt = transformTypeTree(tpt)(owner)
          if ((nInner eq inner) && (nTpt eq tpt)) pattern else TypedOrTest.copy(pattern)(nInner, nTpt)
        case w: Wildcard =>
          w
        case t: Term =>
          transformTerm(t)(owner)
        case other =>
          transformTree(other)(owner)

    /** Transform the extractor pattern. Can be overridden to substitute the extractor. */
    def transformUnapply(u: Unapply)(owner: Symbol): Unapply =
      // read the accessors once: they make new trees on each call for a QuotePattern, and `patterns` makes a new list
      // for an extractor with varargs (appending the elements of the trailing SeqLiteral)
      val fun = u.fun
      val implicits = u.implicits
      val patterns = u.patterns
      val nFun = transformTerm(fun)(owner)
      val nImplicits = transformSubTrees(implicits)(owner)
      val nPatterns = patterns.mapConserve(transformPattern(_)(owner))
      if ((nFun eq fun) && (nImplicits eq implicits) && (nPatterns eq patterns)) u
      else Unapply.copy(u)(nFun, nImplicits, nPatterns)
