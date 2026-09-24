package cps.macros.common

import scala.quoted.*

/** Scope of reflect-level helpers for macro transformations, bound to `qctx`.
  *
  * Helpers which extend `qctx.reflect` members (as [[PatternAwareTreeMapScope.PatternAwareTreeMap]]) are defined in component
  * scopes, mixed here. Macro transformers, situated in `cps.macros.forest.TreeTransformScope`, which extends this scope, use them
  * directly: `new PatternAwareTreeMap { ... }`. Outside of it, use [[MacroReflectScopeInstance]]:
  * {{{
  *   val scope = MacroReflectScopeInstance[quotes.type]
  *   new scope.PatternAwareTreeMap { ... }
  * }}}
  */
trait MacroReflectScope extends PatternAwareTreeMapScope:

  implicit val qctx: Quotes

/** `Q` is the singleton type of the passed `Quotes`, so that `scope.qctx.reflect.Term` unifies with `quotes.reflect.Term` at the
  * call site.
  */
class MacroReflectScopeInstance[Q <: Quotes & Singleton](implicit override val qctx: Q) extends MacroReflectScope
