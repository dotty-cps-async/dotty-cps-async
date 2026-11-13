package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** A match whose cases are changed by the transformation (here an inner lambda is unshifted), but stay sync, is reported as
  * changed, so the changed cases are used (before, the original cases with `await` were kept: "await should be inside async
  * block").
  */
class TestMatchWithChangedSyncCase {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  @Test def testMatchWithChangedSyncCase(): Unit = {
    val c = async[Future] {
      val n = await(Future.successful(1))
      val r = n match {
        case 1 => List(1, 2).map(y => { await(g(y)); Future.successful(y) })
        case _ => Nil
      }
      val values = await(Future.sequence(r))
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

}
