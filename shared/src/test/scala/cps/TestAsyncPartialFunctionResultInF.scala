package cps

import org.junit.{Test, Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Async partial function with the result in the monad: the result of each case is flattened, and elements without a matching
  * case are skipped. See https://github.com/dotty-cps-async/dotty-cps-async/issues/157
  */
class TestAsyncPartialFunctionResultInF {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  @Test def testCollectWithGuard(): Unit = {
    val c = async[Future] {
      val r = List(1, 2, 3).collect { case x if x > 1 => { val y = await(g(x)); Future.successful(y) } }
      val values = await(Future.sequence(r))
      assert(values == List(20, 30), s"values=$values")
    }
    FutureCompleter(c)
  }

  @Test def testCollectWithTypeTest(): Unit = {
    val c = async[Future] {
      val r = List[Any](1, "a", 2).collect { case x: Int => { val y = await(g(x)); Future.successful(y + 1) } }
      val values = await(Future.sequence(r))
      assert(values == List(11, 21), s"values=$values")
    }
    FutureCompleter(c)
  }

}
