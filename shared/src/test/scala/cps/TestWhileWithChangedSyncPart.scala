package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** A `while` whose condition or body is changed by the transformation (here an inner lambda is unshifted), but stays sync, while
  * the other part is async: the changed tree is used (before, the original tree with `await` was kept: "await should be inside async
  * block").
  */
class TestWhileWithChangedSyncPart {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  // the condition is changed but sync, the body is async
  @Test def testWhileWithChangedSyncCondition() = {
    val c = async[Future] {
      var i = 0
      var acc = 0
      while (List(1).map(y => { await(g(y)); Future.successful(y) }).size + i < 4) {
        acc += await(g(i))
        i += 1
      }
      assert(acc == 30, s"acc=$acc")
    }
    FutureCompleter(c)
  }

  // the condition is async, the body is changed but sync
  @Test def testWhileWithChangedSyncBody() = {
    val c = async[Future] {
      var i = 0
      val fs = scala.collection.mutable.ListBuffer[Future[Int]]()
      while (await(Future.successful(i < 2))) {
        fs ++= List(i).map(y => { await(g(y)); Future.successful(y) })
        i += 1
      }
      val values = await(Future.sequence(fs.toList))
      assert(values == List(0, 1), s"values=$values")
    }
    FutureCompleter(c)
  }

}
