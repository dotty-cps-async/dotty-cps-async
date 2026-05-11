package cps.plugin.forest

import cps.plugin.CpsTopLevelContext
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*

object BreaksBreakTransform {

  def apply(term: Apply, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val breaksAsyncShift = Symbols.requiredModule("cps.runtime.util.control.BreaksAsyncShift")
    val breaksModule = Symbols.requiredModule("scala.util.control.Breaks")
    val nApply = Apply(
      Select(ref(breaksAsyncShift), "break".toTermName).withSpan(term.fun.span),
      List(ref(breaksModule))
    ).withSpan(term.span)
    CpsTree.pure(term, owner, nApply)
  }

}
