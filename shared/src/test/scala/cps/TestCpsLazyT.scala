package cps

import org.junit.{Test, Ignore}
import org.junit.Assert._

import scala.util.{Try, Success, Failure}
import scala.util.control.TailCalls._
import scala.util.control.TailCalls

import cps.monads.{CpsIdentity, CpsIdentityMonad, CpsLazy, CpsLazyT}


class TestCpsLazyT:

  @Test def testCpsLazyPureRun(): Unit =
    val la = CpsLazy.pure(42)
    val result = CpsLazy.run(la)
    assertEquals(42, result)

  @Test def testCpsLazyDelayRun(): Unit =
    var counter = 0
    val la = CpsLazy.delay {
      counter += 1
      counter
    }
    assertEquals(0, counter)
    val result = CpsLazy.run(la)
    assertEquals(1, counter)
    assertEquals(1, result)

  @Test def testCpsLazyFlatMapChain(): Unit =
    val la = CpsLazy.pure(10)
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    val lb = monad.flatMap(la)(a => CpsLazy.pure(a + 5))
    val lc = monad.flatMap(lb)(b => CpsLazy.pure(b * 2))
    val result = CpsLazy.run(lc)
    assertEquals(30, result)

  @Test def testCpsLazyErrorHandling(): Unit =
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    val la: CpsLazy[Int] = CpsLazy.error(new RuntimeException("test error"))
    val recovered = monad.flatMapTry(la) {
      case Success(v) => CpsLazy.pure(v)
      case Failure(e) => CpsLazy.pure(-1)
    }
    val result = CpsLazy.run(recovered)
    assertEquals(-1, result)

  @Test def testCpsLazyErrorPropagation(): Unit =
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    val la: CpsLazy[Int] = CpsLazy.error(new RuntimeException("test error"))
    // flatMap should skip over error
    val lb = monad.flatMap(la)((a: Int) => CpsLazy.pure(a + 1))
    // flatMapTry should catch error
    val recovered = monad.flatMapTry(lb) {
      case Success(v) => CpsLazy.pure(v)
      case Failure(e) => CpsLazy.pure(-99)
    }
    val result = CpsLazy.run(recovered)
    assertEquals(-99, result)

  @Test def testCpsLazyStackSafety(): Unit =
    val depth = 100000
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    var la: CpsLazy[Int] = CpsLazy.pure(0)
    for (_ <- 1 to depth) {
      la = monad.flatMap(la)(n => CpsLazy.pure(n + 1))
    }
    val result = CpsLazy.run(la)
    assertEquals(depth, result)

  @Test def testCpsLazyLiftFromIdentity(): Unit =
    val la = CpsLazyT.lift[CpsIdentity, Int](42)
    val result = CpsLazy.run(la)
    assertEquals(42, result)

  @Test def testCpsLazyTWithTailRec(): Unit =
    import cps.monads.given
    type TailRec[A] = TailCalls.TailRec[A]
    val lt = CpsLazyT.pure[TailRec, Int](10)
    val monad = CpsLazyT.cpsLazyTMonad[TailRec]
    val lt2 = monad.flatMap(lt)(a => CpsLazyT.pure[TailRec, Int](a + 5))
    val fResult = CpsLazyT.run[TailRec, Int](lt2)
    val result = fResult.result
    assertEquals(15, result)

  @Test def testCpsLazyEffectMonadDelay(): Unit =
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    var counter = 0
    val la = monad.delay {
      counter += 1
      counter
    }
    assertEquals(0, counter)
    val result = CpsLazy.run(la)
    assertEquals(1, counter)
    assertEquals(1, result)

  @Test def testCpsLazyStackSafetyWithDelay(): Unit =
    val depth = 100000
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)

    def loop(n: Int): CpsLazy[Int] =
      if n <= 0 then CpsLazy.pure(n)
      else CpsLazyT.flatDelay[CpsIdentity, Int](loop(n - 1))

    val la = loop(depth)
    val result = CpsLazy.run(la)
    assertEquals(0, result)

  @Test def testCpsLazyMapPreservesValue(): Unit =
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    val la = CpsLazy.pure("hello")
    val lb = monad.map(la)(_.length)
    val result = CpsLazy.run(lb)
    assertEquals(5, result)

  @Test def testCpsLazyErrorInDelayIsCaught(): Unit =
    val monad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)
    val la: CpsLazy[Int] = monad.delay {
      throw new RuntimeException("boom")
    }
    val recovered = monad.flatMapTry(la) {
      case Success(v) => CpsLazy.pure(v)
      case Failure(e) => CpsLazy.pure(-1)
    }
    val result = CpsLazy.run(recovered)
    assertEquals(-1, result)
