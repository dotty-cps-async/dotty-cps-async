package cps

import org.junit.Test

import scala.concurrent.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

import scala.concurrent.ExecutionContext.Implicits.global

@js.native
@JSGlobalScope
object ApplyTestGlobals extends js.Object {
  def cpsApplyTestTwice(x: Int): Int = js.native
}

/** A call of a function of the Scala.js global scope with an async argument: the global scope object is not a value, it is not
  * bound to a val (see [[TestAssignJSGlobalScopeQualifier]] and https://github.com/dotty-cps-async/dotty-cps-async/issues/169).
  */
class TestApplyJSGlobalScopeQualifier {

  @Test def testJSGlobalScopeFunction() = {
    js.Dynamic.global.globalThis.updateDynamic("cpsApplyTestTwice")(((x: Int) => 2 * x): js.Function1[Int, Int])
    val c = async[Future] {
      val r = ApplyTestGlobals.cpsApplyTestTwice(await(Future.successful(5)))
      assert(r == 10, s"r=$r")
      val d = js.Dynamic.global.cpsApplyTestTwice(await(Future.successful(4)))
      assert(d.asInstanceOf[Int] == 8, s"d=$d")
    }
    FutureCompleter(c)
  }

}
