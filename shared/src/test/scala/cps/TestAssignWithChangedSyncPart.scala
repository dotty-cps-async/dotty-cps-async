package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Assignment where a part is changed by the transformation (here an inner lambda is unshifted), but stays sync: the changed tree is
  * used (before, the original tree with `await` was kept: "await should be inside async block"). Also assignment to a field of an async
  * qualifier (before, the macro failed: the field's singleton type was used as the type of the transformed left part).
  */
class TestAssignWithChangedSyncPart {

  import TestAssignWithChangedSyncPart.*

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  // the right part is changed but sync
  @Test def testAssignChangedSyncRight() = {
    val c = async[Future] {
      var fs: List[Future[Int]] = Nil
      fs = List(1, 2).map(y => { await(g(y)); Future.successful(y) })
      val values = await(Future.sequence(fs))
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // the assigned variable has a literal type, the right part is async: the type of the right part is not widened
  @Test def testAssignAsyncRightToLiteralTypedVar() = {
    val c = async[Future] {
      var s: "a" | "b" = "a"
      s = await(Future.successful["b"]("b"))
      assert(s == "b", s"s=$s")
    }
    FutureCompleter(c)
  }

  // the qualifier of the left part is changed but sync, the right part is sync
  @Test def testAssignChangedSyncQualifier() = {
    val c = async[Future] {
      val h = new Holder
      holder(h, List(1, 2).map(y => { await(g(y)); Future.successful(y) })).n = 3
      val values = await(Future.sequence(h.fs))
      assert(h.n == 3, s"h.n=${h.n}")
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // the qualifier of the left part is changed but sync, the right part is async
  @Test def testAssignChangedSyncQualifierAsyncRight() = {
    val c = async[Future] {
      val h = new Holder
      holder(h, List(1, 2).map(y => { await(g(y)); Future.successful(y) })).n = await(g(3))
      val values = await(Future.sequence(h.fs))
      assert(h.n == 30, s"h.n=${h.n}")
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // the qualifier is async, the right part is changed but sync
  @Test def testAssignAsyncQualifierChangedSyncRight() = {
    val c = async[Future] {
      val h = new Holder
      await(Future.successful(h)).fs = List(1, 2).map(y => { await(g(y)); Future.successful(y) })
      val values = await(Future.sequence(h.fs))
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // the qualifier is async, the right part is sync
  @Test def testAssignAsyncQualifier() = {
    val c = async[Future] {
      val h = new Holder
      await(Future.successful(h)).n = 3
      assert(h.n == 3, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier and the right part are async
  @Test def testAssignAsyncQualifierAsyncRight() = {
    val c = async[Future] {
      val h = new Holder
      await(Future.successful(h)).n = await(g(3))
      assert(h.n == 30, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

}

object TestAssignWithChangedSyncPart {

  class Holder {
    var n: Int = 0
    var fs: List[Future[Int]] = Nil
  }

  def holder(h: Holder, fs: List[Future[Int]]): Holder = {
    h.fs = fs
    h
  }

}
