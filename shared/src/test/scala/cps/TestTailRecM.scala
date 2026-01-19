package cps

import org.junit.{Test, Ignore}
import org.junit.Assert._

import cps.monads.{CpsIdentity, CpsIdentityMonad, IterableCpsMonad, EitherCpsMonad, EitherCpsTryMonad, ThrowableMapping}

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

  // Tests for IterableCpsMonad
  val listMonad = IterableCpsMonad[List](List)

  @Test def testIterableTailRecMSimple(): Unit =
    // Simple countdown in List context
    val result = listMonad.tailRecM[Int, Int](3) { n =>
      if n <= 0 then List(Right(n))
      else List(Left(n - 1))
    }
    assertEquals(List(0), result)

  @Test def testIterableTailRecMBranching(): Unit =
    // Binary tree traversal: each step can branch into two paths
    val result = listMonad.tailRecM[Int, Int](2) { n =>
      if n <= 0 then List(Right(n))
      else List(Left(n - 1), Left(n - 1))  // branch into two
    }
    // 2 -> [1,1] -> [0,0,0,0] = 4 zeros
    assertEquals(List(0, 0, 0, 0), result)

  @Test def testIterableTailRecMStackSafety(): Unit =
    // Deep recursion that would overflow without explicit stack
    val depth = 100000
    val result = listMonad.tailRecM[Int, Int](depth) { n =>
      if n <= 0 then List(Right(n))
      else List(Left(n - 1))
    }
    assertEquals(List(0), result)

  @Test def testIterableTailRecMMixedResults(): Unit =
    // Some branches terminate early, others continue
    val result = listMonad.tailRecM[Int, Int](3) { n =>
      if n <= 0 then List(Right(100))
      else if n == 2 then List(Right(200), Left(n - 1))  // emit 200 and continue
      else List(Left(n - 1))
    }
    // 3 -> [2] -> [200, 1] -> [200, 0] -> [200, 100]
    assertEquals(List(200, 100), result)

  @Test def testIterableTailRecMEmptyBranch(): Unit =
    // Some branches return empty collection (pruning)
    val result = listMonad.tailRecM[Int, Int](3) { n =>
      if n <= 0 then List(Right(n))
      else if n == 2 then List()  // this branch is pruned
      else List(Left(n - 1), Left(n - 2))  // branch into two
    }
    // Stack-based processing: 3 -> pending [1,2] -> process 1 -> pending [-1,0,2]
    // -> -1 emits, 0 emits, 2 pruned
    assertEquals(List(-1, 0), result)

  @Test def testIterableTailRecMAllEmpty(): Unit =
    // All branches eventually return empty
    val result = listMonad.tailRecM[Int, Int](2) { n =>
      if n <= 0 then List()  // all terminal branches pruned
      else List(Left(n - 1))
    }
    assertEquals(List(), result)

  // Tests for EitherCpsMonad
  import scala.util.NotGiven
  val eitherMonad = new EitherCpsMonad[String](using NotGiven.default)

  @Test def testEitherTailRecMSimple(): Unit =
    val result = eitherMonad.tailRecM[Int, Int](5) { n =>
      if n <= 0 then Right(Right(n))
      else Right(Left(n - 1))
    }
    assertEquals(Right(0), result)

  @Test def testEitherTailRecMWithError(): Unit =
    val result = eitherMonad.tailRecM[Int, Int](5) { n =>
      if n == 3 then Left("error at 3")
      else if n <= 0 then Right(Right(n))
      else Right(Left(n - 1))
    }
    assertEquals(Left("error at 3"), result)

  @Test def testEitherTailRecMStackSafety(): Unit =
    val depth = 100000
    val result = eitherMonad.tailRecM[Int, Int](depth) { n =>
      if n <= 0 then Right(Right(n))
      else Right(Left(n - 1))
    }
    assertEquals(Right(0), result)

  // Tests for EitherCpsTryMonad (with Throwable error type)
  val eitherTryMonad = new EitherCpsTryMonad[RuntimeException]

  @Test def testEitherTryTailRecMSimple(): Unit =
    val result = eitherTryMonad.tailRecM[Int, Int](5) { n =>
      if n <= 0 then Right(Right(n))
      else Right(Left(n - 1))
    }
    assertEquals(Right(0), result)

  @Test def testEitherTryTailRecMWithError(): Unit =
    val ex = new RuntimeException("error at 3")
    val result = eitherTryMonad.tailRecM[Int, Int](5) { n =>
      if n == 3 then Left(ex)
      else if n <= 0 then Right(Right(n))
      else Right(Left(n - 1))
    }
    assertEquals(Left(ex), result)

  @Test def testEitherTryTailRecMStackSafety(): Unit =
    val depth = 100000
    val result = eitherTryMonad.tailRecM[Int, Int](depth) { n =>
      if n <= 0 then Right(Right(n))
      else Right(Left(n - 1))
    }
    assertEquals(Right(0), result)

