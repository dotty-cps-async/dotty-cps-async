package cps.macros.common

import scala.quoted.*

/** Holder of [[PatternTreeMap]] for the given `Quotes`.
  *
  * Usage:
  * {{{
  *   val pm = PatternMaps[quotes.type](quotes)
  *   val treeMap = new pm.PatternTreeMap {
  *     override def transformTerm(tree: Term)(owner: Symbol): Term = ...
  *   }
  * }}}
  *
  * `PatternTreeMap` is nested, because a top-level `class PatternTreeMap[Q <: Quotes & Singleton](using val q: Q) extends
  * q.reflect.TreeMap` is unusable: inherited methods are typed with `PatternTreeMap.this.q.reflect.Term`, which does not unify
  * with `quotes.reflect.Term` at the call site. Here they are typed with `pm.q.reflect.Term`, where `pm.q: quotes.type`.
  */
class PatternMaps[Q <: Quotes & Singleton](val q: Q):
  import q.reflect.*

  /** TreeMap which parses the pattern of a `CaseDef` top-down, passing to `transformTerm` and `transformTypeTree` only terms
    * (extractors, implicits, literal and stable-identifier patterns) and type trees. Macro TreeMaps which traverse user code
    * should extend it.
    *
    * The default TreeMap dispatches subpatterns via `transformTree`, where the named pattern `case C(name = p)`, typed as
    * `NamedArg(name, p)` inside `Unapply`, is handled as a term, and `transformTerm` fails on `p` with MatchError (see
    * tests-cli/t2026_09_24_named_pattern_treemap). Also the default TreeMap does not go inside the pattern of `Bind`.
    */
  abstract class PatternTreeMap extends TreeMap:

    override def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef =
      CaseDef.copy(tree)(
        transformPattern(tree.pattern)(owner),
        tree.guard.map(transformTerm(_)(owner)),
        transformTerm(tree.rhs)(owner)
      )

    def transformPattern(pattern: Tree)(owner: Symbol): Tree =
      pattern match
        case b: Bind =>
          Bind.copy(b)(b.name, transformPattern(b.pattern)(owner))
        case u: Unapply =>
          transformUnapply(u)(owner)
        case a: Alternatives =>
          Alternatives.copy(a)(a.patterns.map(transformPattern(_)(owner)))
        case na: NamedArg =>
          // value of NamedArg is a pattern, not a term.
          NamedArg.copy(na)(na.name, transformPattern(na.value)(owner).asInstanceOf[Term])
        case TypedOrTest(inner, tpt) =>
          TypedOrTest.copy(pattern)(transformPattern(inner)(owner), transformTypeTree(tpt)(owner))
        case w: Wildcard =>
          w
        case t: Term =>
          transformTerm(t)(owner)
        case other =>
          transformTree(other)(owner)

    /** Transform the extractor pattern. Can be overridden to substitute the extractor. */
    def transformUnapply(u: Unapply)(owner: Symbol): Unapply =
      Unapply.copy(u)(
        transformTerm(u.fun)(owner),
        transformSubTrees(u.implicits)(owner),
        u.patterns.map(transformPattern(_)(owner))
      )
