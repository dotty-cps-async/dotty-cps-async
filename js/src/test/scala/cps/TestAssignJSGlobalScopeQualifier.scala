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
object AssignTestGlobals extends js.Object {
  var cpsAssignTestGlobal: Int = js.native
}

/** Assignment to a variable of the Scala.js global scope with an async right part: the global scope object is not a value, it is not
  * bound to a val (the backend rejects "Loading the global scope as a value").
  */
class TestAssignJSGlobalScopeQualifier {

  @Test def testJSGlobalScopeVariable() = {
    // a property of `globalThis`: a bare assignment to an undeclared global fails in strict mode
    js.Dynamic.global.globalThis.updateDynamic("cpsAssignTestGlobal")(0)
    val c = async[Future] {
      AssignTestGlobals.cpsAssignTestGlobal = await(Future.successful(5))
      assert(AssignTestGlobals.cpsAssignTestGlobal == 5, s"value=${AssignTestGlobals.cpsAssignTestGlobal}")
    }
    FutureCompleter(c)
  }

}
