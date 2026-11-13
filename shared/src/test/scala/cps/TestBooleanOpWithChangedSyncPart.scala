package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** `&&` / `||` with an operand which is changed by the transformation (here an inner lambda is unshifted), but stays sync: the changed
  * tree is used (before, the original tree with `await` was kept: "await should be inside async block").
  */
class TestBooleanOpWithChangedSyncPart {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  // the left operand is changed, the right is unchanged
  @Test def testAndWithChangedSyncLeft() = {
    val c = async[Future] {
      val b = List(1).map(y => { await(g(y)); Future.successful(y) }).nonEmpty && true
      assert(b)
    }
    FutureCompleter(c)
  }

  // the right operand is changed, the left is unchanged
  @Test def testOrWithChangedSyncRight() = {
    val c = async[Future] {
      val b = false || List(1).map(y => { await(g(y)); Future.successful(y) }).nonEmpty
      assert(b)
    }
    FutureCompleter(c)
  }

  // the left operand is changed but sync, the right is async
  @Test def testAndWithChangedSyncLeftAsyncRight() = {
    val c = async[Future] {
      val b = List(1).map(y => { await(g(y)); Future.successful(y) }).nonEmpty && await(Future.successful(true))
      assert(b)
    }
    FutureCompleter(c)
  }

}
