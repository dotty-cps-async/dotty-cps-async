package cps.logic

import org.junit.{Test, Ignore}
import org.junit.Assert._
import cps.*
import cps.monads.logic.{*, given}

class LazyListTailRecMTest {

  @Test
  def testTailRecMSimple(): Unit = {
    // Simple countdown: 5 -> 4 -> 3 -> 2 -> 1 -> 0
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](5) { n =>
      if n <= 0 then LazyList(Right(n))
      else LazyList(Left(n - 1))
    }
    assertEquals(List(0), result.toList)
  }

  @Test
  def testTailRecMWithAccumulator(): Unit = {
    // Sum 1 to 10 using tailRecM
    val result = LazyListCpsLogicMonad.tailRecM[(Int, Int), Int]((1, 0)) { case (n, acc) =>
      if n > 10 then LazyList(Right(acc))
      else LazyList(Left((n + 1, acc + n)))
    }
    assertEquals(List(55), result.toList)
  }

  @Test
  def testTailRecMStackSafety(): Unit = {
    // Deep recursion that would overflow without proper stack-safe implementation
    val depth = 100000
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](depth) { n =>
      if n <= 0 then LazyList(Right(n))
      else LazyList(Left(n - 1))
    }
    assertEquals(List(0), result.toList)
  }

  @Test
  def testTailRecMBranching(): Unit = {
    // Binary tree traversal: each step can branch into two paths
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](2) { n =>
      if n <= 0 then LazyList(Right(n))
      else LazyList(Left(n - 1), Left(n - 1)) // branch into two
    }
    // 2 -> [1,1] -> [0,0,0,0] = 4 zeros
    assertEquals(List(0, 0, 0, 0), result.toList)
  }

  @Test
  def testTailRecMMixedResults(): Unit = {
    // Some branches terminate early, others continue
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](3) { n =>
      if n <= 0 then LazyList(Right(100))
      else if n == 2 then LazyList(Right(200), Left(n - 1)) // emit 200 and continue
      else LazyList(Left(n - 1))
    }
    // 3 -> [2] -> [200, 1] -> [200, 0] -> [200, 100]
    assertEquals(List(200, 100), result.toList)
  }

  @Test
  def testTailRecMEmptyBranch(): Unit = {
    // Some branches return empty collection (pruning)
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](3) { n =>
      if n <= 0 then LazyList(Right(n))
      else if n == 2 then LazyList.empty // this branch is pruned
      else LazyList(Left(n - 1), Left(n - 2)) // branch into two
    }
    // Depth-first: 3 -> [Left(2), Left(1)] -> 2 pruned -> 1 -> [Left(0), Left(-1)]
    // -> 0 emits first, then -1
    assertEquals(List(0, -1), result.toList)
  }

  @Test
  def testTailRecMAllEmpty(): Unit = {
    // All branches eventually return empty
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](2) { n =>
      if n <= 0 then LazyList.empty // all terminal branches pruned
      else LazyList(Left(n - 1))
    }
    assertEquals(List(), result.toList)
  }

  @Test
  def testTailRecMFactorial(): Unit = {
    // Factorial using tailRecM: fact(10) = 3628800
    def factorial(n: Int): Int =
      LazyListCpsLogicMonad.tailRecM[(Int, Int), Int]((n, 1)) { case (k, acc) =>
        if k <= 1 then LazyList(Right(acc))
        else LazyList(Left((k - 1, acc * k)))
      }.head

    assertEquals(3628800, factorial(10))
    assertEquals(1, factorial(0))
    assertEquals(1, factorial(1))
  }

  @Test
  def testTailRecMLaziness(): Unit = {
    // Verify that subsequent results are computed lazily
    var evaluationCount = 0
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](3) { n =>
      evaluationCount += 1
      if n <= 0 then LazyList(Right(n), Right(n + 100)) // two results at terminal
      else LazyList(Left(n - 1))
    }

    // Implementation processes depth-first to find first result
    // 3 -> 2 -> 1 -> 0 -> Right(0), Right(100)
    assertEquals(4, evaluationCount) // f called for 3, 2, 1, 0

    // Force the first element
    val first = result.head
    assertEquals(0, first)
    assertEquals(4, evaluationCount) // no additional calls

    // Force all elements - still no additional f calls since all paths exhausted
    val all = result.toList
    assertEquals(List(0, 100), all)
    assertEquals(4, evaluationCount)
  }

  @Test
  def testTailRecMBranchingLaziness(): Unit = {
    // With branching, verify that only necessary branches are evaluated
    var evaluationCount = 0
    val result = LazyListCpsLogicMonad.tailRecM[Int, Int](2) { n =>
      evaluationCount += 1
      if n <= 0 then LazyList(Right(n * 10))
      else LazyList(Left(n - 1), Left(n + 10)) // branch: n-1 (toward 0) and n+10 (away)
    }

    // Depth-first: 2 -> [Left(1), Left(12)] -> 1 -> [Left(0), Left(11)] -> 0 -> Right(0)
    // Only f(2), f(1), f(0) evaluated to find first result
    assertEquals(3, evaluationCount)

    val first = result.head
    assertEquals(0, first)
    assertEquals(3, evaluationCount)

    // Force more elements - will evaluate remaining branches
    val all = result.take(4).toList
    // Continues from pending iterators, eventually reaching terminals
    assertTrue(all.contains(0))
  }

}
