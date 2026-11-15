package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** A call of a Java static method with an async argument: the statics object of a Java class is not a value, it is not bound to a
  * val (see [[TestAssignJavaStaticQualifier]] and https://github.com/dotty-cps-async/dotty-cps-async/issues/169).
  */
class TestApplyJavaStaticQualifier {

  @Test def testJavaStaticMethod() = {
    val c = async[Future] {
      val r = java.lang.Math.max(await(Future.successful(5)), 3)
      assert(r == 5, s"r=$r")
    }
    FutureCompleter(c)
  }

}
