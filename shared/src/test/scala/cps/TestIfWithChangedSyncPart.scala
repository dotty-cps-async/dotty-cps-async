package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** An `if` whose condition or branch is changed by the transformation (here an inner lambda is unshifted), but stays sync, while
  * another part is async: the changed tree is used (before, the original tree with `await` was kept: "await should be inside async
  * block").
  */
class TestIfWithChangedSyncPart {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  // the condition is changed but sync, the branches are async
  @Test def testIfWithChangedSyncCondition() = {
    val c = async[Future] {
      val r =
        if (List(1).map(y => { await(g(y)); Future.successful(y) }).nonEmpty) await(g(1))
        else await(g(2))
      assert(r == 10, s"r=$r")
    }
    FutureCompleter(c)
  }

  // the condition is async, a branch is changed but sync
  @Test def testIfWithChangedSyncBranch() = {
    val c = async[Future] {
      val r =
        if (await(Future.successful(true))) List(1, 2).map(y => { await(g(y)); Future.successful(y) })
        else Nil
      val values = await(Future.sequence(r))
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // the condition is async, the else branch is changed but sync
  @Test def testIfWithChangedSyncElseBranch() = {
    val c = async[Future] {
      val r =
        if (await(Future.successful(false))) Nil
        else List(1, 2).map(y => { await(g(y)); Future.successful(y) })
      val values = await(Future.sequence(r))
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

}
