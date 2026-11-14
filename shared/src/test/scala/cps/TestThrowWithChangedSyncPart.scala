package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** `throw` with an argument which is changed by the transformation (here an inner lambda is unshifted), but stays sync: the changed tree
  * is used (before, the original tree with `await` was kept: "await should be inside async block").
  */
class TestThrowWithChangedSyncPart {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  @Test def testThrowWithChangedSyncArgument() = {
    val c = async[Future] {
      if (await(g(1)) > 0) then throw new RuntimeException(List(1, 2).map(y => { await(g(y)); Future.successful(y) }).size.toString)
      0
    }
    FutureCompleter(c.failed.map(ex => assert(ex.getMessage == "2", s"message=${ex.getMessage}")))
  }

}
