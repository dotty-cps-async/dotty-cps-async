package cps.plugin.forest

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*

/** Rewrites `case NonFatal(_)` extractors to use
  * `NonFatalAndNotControlThrowableAsyncWrapper.unapply`, so a user catch inside a
  * shifted `breakable` / `returning` body does not swallow the wrapped
  * `ControlThrowable` we route through the monad.
  */
object NonFatalSubstitution {

  def apply(tree: Tree)(using Context): Tree = {
    val nonFatalUnapplySym = Symbols.requiredClass("scala.util.control.NonFatal$").requiredMethod("unapply")
    val wrapperObj = ref(
      Symbols.requiredModule("cps.runtime.util.control.NonFatalAndNotControlThrowableAsyncWrapper")
    )
    val mapper = new TreeMap {
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case u: UnApply if u.fun.symbol == nonFatalUnapplySym =>
            val nFun = Select(wrapperObj, "unapply".toTermName)
            cpy.UnApply(u)(nFun, u.implicits, u.patterns)
          case _ =>
            super.transform(tree)
    }
    mapper.transform(tree)
  }

}
