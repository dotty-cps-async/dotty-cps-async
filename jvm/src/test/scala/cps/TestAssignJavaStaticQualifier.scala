package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Assignment to a Java static field with an async right part: the statics object of a Java class is not a value, it is not bound to a
  * val. (A first version of the #165 fix bound it, and the compiler backend crashed: "an unexpected type representation reached the
  * compiler backend".)
  */
class TestAssignJavaStaticQualifier {

  @Test def testJavaStaticField() = {
    val c = async[Future] {
      AssignTestJavaStatics.counter = await(Future.successful(5))
      assert(AssignTestJavaStatics.counter == 5, s"counter=${AssignTestJavaStatics.counter}")
    }
    FutureCompleter(c)
  }

}
