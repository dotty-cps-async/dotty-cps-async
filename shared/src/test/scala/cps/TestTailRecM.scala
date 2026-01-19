package cps

import org.junit.{Test, Ignore}
import org.junit.Assert._

import cps.monads.{CpsIdentity, CpsIdentityMonad}

class TestTailRecM:

  @Test def testTailRecMSimple(): Unit =
    // Simple countdown: 5 -> 4 -> 3 -> 2 -> 1 -> 0
    val result = CpsIdentityMonad.tailRecM[Int, Int](5) { n =>
      if n <= 0 then Right(n)
      else Left(n - 1)
    }
    assertEquals(0, result)

  @Test def testTailRecMWithAccumulator(): Unit =
    // Sum 1 to 10 using tailRecM
    val result = CpsIdentityMonad.tailRecM[(Int, Int), Int]((1, 0)) { case (n, acc) =>
      if n > 10 then Right(acc)
      else Left((n + 1, acc + n))
    }
    assertEquals(55, result)

  @Test def testTailRecMStackSafety(): Unit =
    // Deep recursion that would overflow without proper tail recursion
    val depth = 100000
    val result = CpsIdentityMonad.tailRecM[Int, Int](depth) { n =>
      if n <= 0 then Right(n)
      else Left(n - 1)
    }
    assertEquals(0, result)

  @Test def testTailRecMFactorial(): Unit =
    // Factorial using tailRecM: fact(10) = 3628800
    def factorial(n: Int): Int =
      CpsIdentityMonad.tailRecM[(Int, Int), Int]((n, 1)) { case (k, acc) =>
        if k <= 1 then Right(acc)
        else Left((k - 1, acc * k))
      }
    assertEquals(3628800, factorial(10))
    assertEquals(1, factorial(0))
    assertEquals(1, factorial(1))

