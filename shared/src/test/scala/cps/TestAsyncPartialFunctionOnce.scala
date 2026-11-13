package cps

import org.junit.{Test, Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.*

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Guards and extractors of an async partial function are evaluated once, as for a partial function literal (in
  * `applyOrElse`). See https://github.com/dotty-cps-async/dotty-cps-async/issues/155
  */
object TestAsyncPartialFunctionOnce {

  class Counter {
    var calls = 0
    def check(b: Boolean): Boolean =
      calls += 1
      b
  }

  class EvenExtractor {
    var calls = 0
    def unapply(x: Int): Option[Int] =
      calls += 1
      if x % 2 == 0 then Some(x / 2) else None
  }

  class Pipe(xs: List[Int]) {
    def collectMap(pf: PartialFunction[Int, Int], f: Int => Int): List[Int] = xs.collect(pf).map(f)
    def collectMap_async[F[_]](m: CpsMonad[F], pf: PartialFunction[Int, F[Int]], f: Int => F[Int]): F[List[Int]] =
      xs.collect(pf).foldRight(m.pure(List.empty[Int])) { (fv, acc) =>
        m.flatMap(fv)(v => m.flatMap(f(v))(r => m.map(acc)(r :: _)))
      }
  }

}

class TestAsyncPartialFunctionOnce {

  import TestAsyncPartialFunctionOnce.*

  @Test def testCollectGuardWithSideEffects() = {
    val counter = Counter()
    // true on odd calls: a guard evaluated twice for the same element changes its result
    def tick(): Boolean = counter.check(counter.calls % 2 == 0)
    val c = async[Future] {
      val r = List(1, 2, 3).collect { case x if tick() => await(Future.successful(x)) }
      assert(r == List(1, 3), s"r=$r")
      assert(counter.calls == 3, s"calls=${counter.calls}")
    }
    FutureCompleter(c)
  }

  @Test def testCollectExtractorWithSideEffects() = {
    val Even = EvenExtractor()
    val c = async[Future] {
      val r = List(1, 2, 3, 4).collect { case Even(h) => await(Future.successful(h)) }
      assert(r == List(1, 2), s"r=$r")
      assert(Even.calls == 4, s"calls=${Even.calls}")
    }
    FutureCompleter(c)
  }

  @Test def testCollectFirst() = {
    val Even = EvenExtractor()
    val c = async[Future] {
      val r = List(1, 2, 3, 4).collectFirst { case Even(h) if h > 1 => await(Future.successful(h)) }
      assert(r == Some(2), s"r=$r")
      assert(Even.calls == 4, s"calls=${Even.calls}")
    }
    FutureCompleter(c)
  }

  @Test def testOptionCollect() = {
    val Even = EvenExtractor()
    val c = async[Future] {
      val r = Option(4).collect { case Even(h) => await(Future.successful(h)) }
      assert(r == Some(2), s"r=$r")
      assert(Even.calls == 1, s"calls=${Even.calls}")
    }
    FutureCompleter(c)
  }

  @Test def testTryRecover() = {
    val counter = Counter()
    val failed: Try[Int] = Failure(new RuntimeException("e"))
    val c = async[Future] {
      val r = failed.recover { case _: RuntimeException if counter.check(true) => await(Future.successful(5)) }
      assert(r == Success(5), s"r=$r")
      assert(counter.calls == 1, s"calls=${counter.calls}")
    }
    FutureCompleter(c)
  }

  // the sync partial function is shifted because of the async lambda
  @Test def testSyncPartialFunctionWithAsyncLambda() = {
    val counter = Counter()
    val c = async[Future] {
      val r = Pipe(List(1, 2, 3)).collectMap({ case x if counter.check(x % 2 == 0) => x * 10 }, y => y + await(Future.successful(y)))
      assert(r == List(40), s"r=$r")
      assert(counter.calls == 3, s"calls=${counter.calls}")
    }
    FutureCompleter(c)
  }

}
