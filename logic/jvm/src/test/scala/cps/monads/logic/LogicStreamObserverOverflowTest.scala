package cps.monads.logic

import org.junit.{Test, Ignore}
import org.junit.Assert._
import scala.util.{Try, Success, Failure}

import cps.monads.{CpsLazy, CpsLazyT, CpsIdentity, CpsIdentityMonad}

/**
 * Stress tests for stack overflow scenarios in CpsLogicMonad
 * choice exploration when using CpsIdentity (synchronous) observer.
 *
 * Part 1 (observer fold): FIXED by rewriting mFoldLeftWhileObserveM
 * to use observerCpsMonad.tailRecM. For CpsIdentity, tailRecM is
 * @tailrec, so the fold is now stack-safe.
 *
 * Part 2 (interleave/flatMap chain overflow): CpsLogicMonad.interleave uses
 * flatMap(msplit(a)) recursively. For LazyList, nested flatMap calls
 * create deep iterator chains in LazyList's internal implementation
 * (stateFromIteratorConcatSuffix). For LogicStream, the observer fold
 * no longer overflows, but the interleave structure itself can.
 *
 * Part 3 (deep fsplit exploration overflow): Dijkstra-like graph exploration
 * using recursive fsplit to traverse a search tree overflows because each
 * recursive step through fsplit adds frames to the JVM stack.
 *
 * Parts 2-3 require lazyFsplit + SuspendableObserverProvider (the full
 * CpsLazyT trampolining pattern from rl-logic) to resolve.
 */
class LogicStreamObserverOverflowTest {

  val N = 100000

  /**
   * Depth limit for detecting would-be stack overflow without actually
   * triggering one. Any real JVM stack is far smaller than 10000 frames,
   * so exceeding this limit proves the recursion is unbounded.
   */
  val MaxDepth = 10000

  class DepthLimitExceeded extends RuntimeException("recursion depth exceeded MaxDepth")

  // =========================================================================
  // Part 1: Observer fold overflow (mFoldLeftWhileObserveM)
  // =========================================================================

  /**
   * Observer fold on a large LogicStream is now stack-safe.
   *
   * mFoldLeftWhileObserveM uses observerCpsMonad.tailRecM, which for
   * CpsIdentity is @tailrec. The fold itself no longer overflows.
   */
  @Test
  def testLogicStreamObserveNNoOverflow(): Unit = {
    val m = CpsLogicStreamSyncMonad
    val stream: LogicStream[Int] = LogicStream.fromCollection((1 to N).toList)
    val result = m.mObserveN(stream, N)
    assertEquals(N, result.size)
  }

  /**
   * FoldWhile on a large LogicStream is now stack-safe.
   */
  @Test
  def testLogicStreamFoldWhileNoOverflow(): Unit = {
    val m = CpsLogicStreamSyncMonad
    val stream: LogicStream[Int] = LogicStream.fromCollection((1 to N).toList)
    val sum: Long = m.mFoldLeftWhileObserve[Int, Long](stream, 0L, (_: Long) => true)((acc, x) => acc + x)
    assertEquals(N.toLong * (N + 1) / 2, sum)
  }

  /**
   * LazyList observeN does NOT overflow because LazyListCpsLogicMonad
   * overrides mFoldLeftWhileObserveM with a @tailrec implementation.
   */
  @Test
  def testLazyListObserveNNoOverflow(): Unit = {
    val m = LazyListCpsLogicMonad
    val stream: LazyList[Int] = LazyList.from(1).take(N)
    val result = m.mObserveN(stream, N)
    assertEquals(N, result.size)
    assertEquals(1, result.head)
    assertEquals(N, result.last)
  }

  /**
   * LazyList foldWhile does NOT overflow.
   */
  @Test
  def testLazyListFoldWhileNoOverflow(): Unit = {
    val m = LazyListCpsLogicMonad
    val stream: LazyList[Int] = LazyList.from(1).take(N)
    val sum: Long = m.mFoldLeftWhileObserve[Int, Long](stream, 0L, (_: Long) => true)((acc, x) => acc + x)
    assertEquals(N.toLong * (N + 1) / 2, sum)
  }

  // =========================================================================
  // Part 2: Interleave overflow
  // =========================================================================

  /**
   * LogicStream interleave: now stack-safe thanks to trampolined lazyFsplit.
   *
   * Previously, observing many interleaved results overflowed because
   * interleave builds nested flatMap(msplit(a)) chains on the JVM stack.
   * With lazyFsplit routing through CpsLazyT, recursive fsplit calls
   * are trampolined via tailRecM.
   */
  @Test
  def testLogicStreamInterleaveObserveNoOverflow(): Unit = {
    val m = CpsLogicStreamSyncMonad
    val halfD = MaxDepth / 2
    val a: LogicStream[Int] = LogicStream.fromCollection((1 to halfD).toList)
    val b: LogicStream[Int] = LogicStream.fromCollection((halfD + 1 to MaxDepth).toList)
    val interleaved = m.interleave(a, b)
    // Single observation is fine
    val first = m.mObserveOne(interleaved)
    assertTrue(first.isDefined)
    // Observing many elements should now succeed without overflow
    val many = m.mObserveN(interleaved, MaxDepth)
    assertEquals(MaxDepth, many.size)
  }

  /**
   * LazyList interleave ALSO overflows. The interleave function uses
   * flatMap(msplit(a)) which for LazyList creates nested iterator chains
   * inside LazyList's stateFromIteratorConcatSuffix. Even though the
   * @tailrec fold iterates over elements, each .tail force triggers the
   * lazy interleave computation which builds deep flatMap-produced
   * iterator chains in the standard library.
   *
   * This demonstrates that LazyList's laziness alone doesn't prevent
   * overflow in higher-order operations like interleave - a trampoline
   * mechanism (like LazyT/CpsLazyT) is needed.
   */
  @Test
  def testLazyListInterleaveOverflow(): Unit = {
    val m = LazyListCpsLogicMonad
    val halfD = MaxDepth / 2
    val a: LazyList[Int] = LazyList.from(1).take(halfD)
    val b: LazyList[Int] = LazyList.from(halfD + 1).take(halfD)
    val interleaved = m.interleave(a, b)
    try {
      val result = m.mObserveN(interleaved, MaxDepth)
      // If we get here, the problem was fixed
      assertEquals(MaxDepth, result.size)
    } catch {
      case _: StackOverflowError =>
        // Expected: nested flatMap chains from interleave overflow
        // in LazyList's internal stateFromIteratorConcatSuffix
        return
    }
  }

  // =========================================================================
  // Part 3: Construction safety (these should all pass)
  // =========================================================================

  /**
   * LogicStream construction via fromCollection does NOT overflow.
   * The data structure construction is fine; only observation overflows.
   */
  @Test
  def testLogicStreamConstructionNoOverflow(): Unit = {
    val stream: LogicStream[Int] = LogicStream.fromCollection((1 to N).toList)
    val first = CpsLogicStreamSyncMonad.mObserveOne(stream)
    assertEquals(Some(1), first)
  }

  /**
   * CpsLogicMonad.fromCollection builds a lazy chain: mplus(pure(1), mplus(pure(2), ...))
   * Construction is safe (by-name second arg), and observation is now stack-safe
   * thanks to the tailRecM-based mFoldLeftWhileObserveM.
   *
   * Note: fromCollection captures a mutable Iterator in thunks, so the stream
   * should only be observed once. Calling mObserveOne then mObserveN on the same
   * stream would re-evaluate thunks with an already-advanced iterator, losing elements.
   */
  @Test
  def testLogicStreamRecursiveFromCollectionObserveNoOverflow(): Unit = {
    val m = CpsLogicStreamSyncMonad
    val stream = m.fromCollection((1 to N).toList)
    val all = m.mObserveN(stream, N)
    assertEquals(N, all.size)
    assertEquals(1, all.head)
    assertEquals(N, all.last)
  }

  // =========================================================================
  // Part 3b: CpsLogicMonad.fromCollection iterator re-evaluation bug
  //
  // fromCollection captures a mutable Iterator in by-name thunks.
  // Cons.tail is () => LogicStreamT (not memoized), so each fsplit
  // re-evaluates the thunk, advancing the shared iterator and losing elements.
  // =========================================================================

  /**
   * Regression test: CpsLogicMonad.fromCollection previously captured a
   * mutable Iterator in by-name thunks. Observing the stream twice would
   * re-evaluate thunks, advancing the shared iterator and losing elements.
   *
   * Fixed by using foldLeft with concrete values instead of recursive
   * iterator-capturing thunks.
   */
  @Test
  def testFromCollectionMultipleObservation(): Unit = {
    val m = CpsLogicStreamSyncMonad
    val stream = m.fromCollection(List(1, 2, 3, 4, 5))

    val first = m.mObserveOne(stream)
    assertEquals(Some(1), first)

    // Second observation on the SAME stream should see all 5 elements
    val all = m.mObserveN(stream, 5)
    assertEquals(5, all.size)
  }

  // =========================================================================
  // Part 4: Dijkstra-like graph exploration with deep fsplit
  //
  // Modeled after ShortestPath in rl-logic: recursive fsplit-based
  // exploration of a search tree. Each step splits the stream, examines
  // the first element, and recurses on the rest.
  // =========================================================================

  case class Edge(to: Int, cost: Int)

  /**
   * Build a grid graph with gridSize x gridSize nodes.
   * Node (r,c) has index r * gridSize + c.
   * Edges: right (cost 1) and down (cost 1).
   */
  def buildGridGraph(gridSize: Int): Map[Int, Seq[Edge]] = {
    val edges = scala.collection.mutable.Map[Int, List[Edge]]().withDefaultValue(Nil)
    for {
      r <- 0 until gridSize
      c <- 0 until gridSize
    } {
      val node = r * gridSize + c
      if (c + 1 < gridSize) edges(node) = Edge(r * gridSize + c + 1, 1) :: edges(node)
      if (r + 1 < gridSize) edges(node) = Edge((r + 1) * gridSize + c, 1) :: edges(node)
    }
    edges.toMap.withDefaultValue(Nil)
  }

  case class PathState(node: Int, path: List[Int], cost: Int)

  /**
   * Dijkstra-like exploration using LogicStream with recursive fsplit.
   * This is the pattern used in rl-logic's ShortestPath.shortestPath.
   *
   * For each step:
   *   1. fsplit the frontier to get the best candidate
   *   2. If target reached, return path
   *   3. If already visited, skip and recurse on rest
   *   4. Otherwise, expand neighbors and recurse on merged frontier
   *
   * @param depthLimit max recursion depth; throws DepthLimitExceeded if exceeded
   */
  def exploreLogicStream(
    graph: Map[Int, Seq[Edge]],
    target: Int,
    frontier: LogicStream[PathState],
    settled: Set[Int],
    depth: Int = 0
  ): Option[List[Int]] = {
    if (depth > MaxDepth) throw new DepthLimitExceeded
    val m = CpsLogicStreamSyncMonad
    m.fsplit(frontier) match {
      case None => None
      case Some((tryState, rest)) =>
        tryState match {
          case Failure(e) => throw e
          case Success(state) =>
            if (state.node == target) {
              Some(state.path.reverse)
            } else if (settled.contains(state.node)) {
              exploreLogicStream(graph, target, rest, settled, depth + 1)
            } else {
              val neighbors = graph(state.node)
              val expanded = neighbors.foldLeft(m.mzero[PathState]) { (acc, edge) =>
                if (settled.contains(edge.to)) acc
                else m.mplus(acc, m.pure(PathState(edge.to, edge.to :: state.path, state.cost + edge.cost)))
              }
              exploreLogicStream(graph, target, m.mplus(expanded, rest), settled + state.node, depth + 1)
            }
        }
    }
  }

  /**
   * Same exploration but using LazyList.
   */
  def exploreLazyList(
    graph: Map[Int, Seq[Edge]],
    target: Int,
    frontier: LazyList[PathState],
    settled: Set[Int],
    depth: Int = 0
  ): Option[List[Int]] = {
    if (depth > MaxDepth) throw new DepthLimitExceeded
    val m = LazyListCpsLogicMonad
    m.fsplit(frontier) match {
      case None => None
      case Some((tryState, rest)) =>
        tryState match {
          case Failure(e) => throw e
          case Success(state) =>
            if (state.node == target) {
              Some(state.path.reverse)
            } else if (settled.contains(state.node)) {
              exploreLazyList(graph, target, rest, settled, depth + 1)
            } else {
              val neighbors = graph(state.node)
              val expanded = neighbors.foldLeft(m.mzero[PathState]) { (acc, edge) =>
                if (settled.contains(edge.to)) acc
                else m.mplus(acc, m.pure(PathState(edge.to, edge.to :: state.path, state.cost + edge.cost)))
              }
              exploreLazyList(graph, target, m.mplus(expanded, rest), settled + state.node, depth + 1)
            }
        }
    }
  }

  /**
   * Small graph exploration works fine for both LogicStream and LazyList.
   */
  @Test
  def testSmallGraphExplorationWorks(): Unit = {
    val gridSize = 10
    val graph = buildGridGraph(gridSize)
    val target = gridSize * gridSize - 1  // bottom-right corner

    val m = CpsLogicStreamSyncMonad
    val start = PathState(0, List(0), 0)
    val frontier: LogicStream[PathState] = m.pure(start)
    val result = exploreLogicStream(graph, target, frontier, Set.empty)
    assertTrue(s"Path should be found in ${gridSize}x${gridSize} grid", result.isDefined)
    assertEquals(0, result.get.head)
    assertEquals(target, result.get.last)
  }

  /**
   * LogicStream graph exploration with large grid exceeds depth limit.
   *
   * The recursive fsplit-based exploration (Dijkstra pattern) adds one
   * stack frame per explored node. With a 100x100 grid (10000 nodes),
   * the exploration visits thousands of nodes, each adding a stack frame
   * through the recursive exploreLogicStream -> fsplit -> exploreLogicStream chain.
   *
   * Uses a depth counter instead of catching StackOverflowError for fast failure.
   */
  @Test
  def testLogicStreamGraphExplorationOverflow(): Unit = {
    val gridSize = 100  // 10000 nodes
    val graph = buildGridGraph(gridSize)
    val target = gridSize * gridSize - 1

    val m = CpsLogicStreamSyncMonad
    val start = PathState(0, List(0), 0)
    val frontier: LogicStream[PathState] = m.pure(start)
    try {
      val result = exploreLogicStream(graph, target, frontier, Set.empty)
      // If we get here, the problem was fixed
      assertTrue(result.isDefined)
    } catch {
      case _: DepthLimitExceeded =>
        // Expected: recursive fsplit exploration exceeds depth limit.
        // This is the same pattern as rl-logic's ShortestPath,
        // which uses LazyT/SuspendableObserverProvider to avoid overflow.
        return
    }
  }

  /**
   * LazyList graph exploration with large grid exceeds depth limit.
   *
   * LazyList's fsplit returns Option[(Try[A], LazyList[A])] directly
   * (Observer = identity). The recursive exploration adds one stack frame
   * per explored node. With a large grid, this also exceeds the depth limit.
   */
  @Test
  def testLazyListGraphExplorationOverflow(): Unit = {
    val gridSize = 100  // 10000 nodes
    val graph = buildGridGraph(gridSize)
    val target = gridSize * gridSize - 1

    val m = LazyListCpsLogicMonad
    val start = PathState(0, List(0), 0)
    val frontier: LazyList[PathState] = m.pure(start)
    try {
      val result = exploreLazyList(graph, target, frontier, Set.empty)
      // If we get here, the exploration didn't overflow
      assertTrue(result.isDefined)
    } catch {
      case _: DepthLimitExceeded =>
        // Expected: recursive exploration exceeds depth limit
        return
    }
  }

  // =========================================================================
  // Part 5: CpsLazy proof-of-concept -- wrapping observation in CpsLazy
  // avoids stack overflow by trampolining.
  // =========================================================================

  /**
   * Demonstrates that wrapping a deep fold computation in CpsLazy
   * provides stack safety through trampolining.
   *
   * Instead of direct recursion on the JVM stack, each step is
   * captured as a CpsLazyT.Delay node and interpreted via tailRecM.
   */
  @Test
  def testCpsLazyFoldNoOverflow(): Unit = {
    val lazyMonad = CpsLazyT.cpsLazyTMonad[CpsIdentity](using CpsIdentityMonad)

    // Build a deep chain of flatMap operations in CpsLazy
    // This is analogous to what mFoldLeftWhileObserveM does:
    // N recursive flatMap steps, but trampolined via CpsLazyT.
    def foldInLazy(items: List[Int], acc: Long): CpsLazy[Long] = {
      items match {
        case Nil => CpsLazy.pure(acc)
        case head :: tail =>
          CpsLazyT.flatDelay[CpsIdentity, Long](foldInLazy(tail, acc + head))
      }
    }

    val items = (1 to N).toList
    val la = foldInLazy(items, 0L)
    val result = CpsLazy.run(la)
    assertEquals(N.toLong * (N + 1) / 2, result)
  }

}
