package cps.monads.logic

import org.junit.{Test, Ignore}
import org.junit.Assert._
import scala.annotation.tailrec
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
 * Part 2 (interleave/flatMap chain overflow): FIXED for LogicStream by
 * routing fsplit through trampolined lazyFsplit + SuspendableObserverProvider.
 * LazyList interleave still overflows due to internal stateFromIteratorConcatSuffix.
 *
 * Part 3 (deep fsplit exploration): FIXED by making user-level exploration
 * loops @tailrec. Combined with trampolined fsplit, the entire graph
 * exploration is now stack-safe for both LogicStream and LazyList.
 */
class LogicStreamObserverOverflowTest {

  val N = 100000

  /**
   * Depth limit for detecting would-be stack overflow without actually
   * triggering one. Any real JVM stack is far smaller than 10000 frames,
   * so exceeding this limit proves the recursion is unbounded.
   */
  val MaxDepth = 10000

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
   * LazyList interleave is now stack-safe. The override in
   * LazyListCpsLogicMonad uses LazyList.cons directly instead of
   * flatMap(msplit(a)), avoiding the deep iterator chain buildup
   * in LazyList's internal stateFromIteratorConcatSuffix.
   */
  @Test
  def testLazyListInterleaveNoOverflow(): Unit = {
    val m = LazyListCpsLogicMonad
    val halfD = MaxDepth / 2
    val a: LazyList[Int] = LazyList.from(1).take(halfD)
    val b: LazyList[Int] = LazyList.from(halfD + 1).take(halfD)
    val interleaved = m.interleave(a, b)
    val result = m.mObserveN(interleaved, MaxDepth)
    assertEquals(MaxDepth, result.size)
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
   * Dijkstra-like exploration using LogicStream with @tailrec fsplit loop.
   * This is the pattern used in rl-logic's ShortestPath.shortestPath.
   *
   * For each step:
   *   1. fsplit the frontier to get the best candidate
   *   2. If target reached, return path
   *   3. If already visited, skip and recurse on rest
   *   4. Otherwise, expand neighbors and recurse on merged frontier
   *
   * Since fsplit is trampolined via lazyFsplit/CpsLazyT and the user
   * loop is @tailrec, the entire exploration is stack-safe.
   */
  def exploreLogicStream(
    graph: Map[Int, Seq[Edge]],
    target: Int,
    frontier: LogicStream[PathState],
    settled: Set[Int]
  ): Option[List[Int]] = {
    val m = CpsLogicStreamSyncMonad
    @tailrec
    def loop(frontier: LogicStream[PathState], settled: Set[Int]): Option[List[Int]] = {
      m.fsplit(frontier) match {
        case None => None
        case Some((tryState, rest)) =>
          tryState match {
            case Failure(e) => throw e
            case Success(state) =>
              if (state.node == target) {
                Some(state.path.reverse)
              } else if (settled.contains(state.node)) {
                loop(rest, settled)
              } else {
                val neighbors = graph(state.node)
                val expanded = neighbors.foldLeft(m.mzero[PathState]) { (acc, edge) =>
                  if (settled.contains(edge.to)) acc
                  else m.mplus(acc, m.pure(PathState(edge.to, edge.to :: state.path, state.cost + edge.cost)))
                }
                loop(m.mplus(expanded, rest), settled + state.node)
              }
          }
      }
    }
    loop(frontier, settled)
  }

  /**
   * Same exploration but using LazyList.
   */
  def exploreLazyList(
    graph: Map[Int, Seq[Edge]],
    target: Int,
    frontier: LazyList[PathState],
    settled: Set[Int]
  ): Option[List[Int]] = {
    val m = LazyListCpsLogicMonad
    @tailrec
    def loop(frontier: LazyList[PathState], settled: Set[Int]): Option[List[Int]] = {
      m.fsplit(frontier) match {
        case None => None
        case Some((tryState, rest)) =>
          tryState match {
            case Failure(e) => throw e
            case Success(state) =>
              if (state.node == target) {
                Some(state.path.reverse)
              } else if (settled.contains(state.node)) {
                loop(rest, settled)
              } else {
                val neighbors = graph(state.node)
                val expanded = neighbors.foldLeft(m.mzero[PathState]) { (acc, edge) =>
                  if (settled.contains(edge.to)) acc
                  else m.mplus(acc, m.pure(PathState(edge.to, edge.to :: state.path, state.cost + edge.cost)))
                }
                loop(m.mplus(expanded, rest), settled + state.node)
              }
          }
      }
    }
    loop(frontier, settled)
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
   * LogicStream graph exploration with large grid is now stack-safe.
   *
   * The @tailrec exploration loop combined with trampolined fsplit
   * (via lazyFsplit/CpsLazyT) makes the entire exploration stack-safe.
   */
  @Test
  def testLogicStreamGraphExplorationNoOverflow(): Unit = {
    val gridSize = 100  // 10000 nodes
    val graph = buildGridGraph(gridSize)
    val target = gridSize * gridSize - 1

    val m = CpsLogicStreamSyncMonad
    val start = PathState(0, List(0), 0)
    val frontier: LogicStream[PathState] = m.pure(start)
    val result = exploreLogicStream(graph, target, frontier, Set.empty)
    assertTrue("Path should be found in 100x100 grid", result.isDefined)
    assertEquals(0, result.get.head)
    assertEquals(target, result.get.last)
  }

  /**
   * LazyList graph exploration with large grid is now stack-safe.
   *
   * The @tailrec exploration loop does not grow the JVM stack.
   * LazyList's fsplit returns directly (Observer = identity),
   * and the loop is @tailrec, so the exploration is stack-safe.
   */
  @Test
  def testLazyListGraphExplorationNoOverflow(): Unit = {
    val gridSize = 100  // 10000 nodes
    val graph = buildGridGraph(gridSize)
    val target = gridSize * gridSize - 1

    val m = LazyListCpsLogicMonad
    val start = PathState(0, List(0), 0)
    val frontier: LazyList[PathState] = m.pure(start)
    val result = exploreLazyList(graph, target, frontier, Set.empty)
    assertTrue("Path should be found in 100x100 grid", result.isDefined)
    assertEquals(0, result.get.head)
    assertEquals(target, result.get.last)
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
  // =========================================================================
  // Part 6: LazyList filter -- now stack-safe via withMsplit
  //
  // The `filter` extension method on CpsLogicMonad previously used
  // flatMap(msplit(a)) { ... } recursively. For LazyList, each step
  // wrapped the result in a flatMap on a singleton LazyList, building
  // deep stateFromIteratorConcatSuffix chains. Now that filter uses
  // withMsplit, which for LazyList (Observer=Identity) becomes a direct
  // call f(fsplit(c)), the singleton flatMap is eliminated.
  // =========================================================================

  /**
   * LazyList filter is now stack-safe thanks to withMsplit.
   *
   * Previously each element processed by filter added a flatMap(msplit(...))
   * layer, causing StackOverflowError. With withMsplit, the singleton
   * flatMap is eliminated and filtering is stack-safe.
   */
  @Test
  def testLazyListFilterNoOverflow(): Unit = {
    val m = LazyListCpsLogicMonad
    val stream: LazyList[Int] = LazyList.from(1).take(MaxDepth)
    import cps.monads.logic.filter
    val filtered = filter[LazyList, Int](stream)(using m)(_ % 2 == 0)
    val result = m.mObserveN(filtered, MaxDepth / 2)
    assertEquals(MaxDepth / 2, result.size)
  }

  // =========================================================================
  // Part 7: CpsLazy proof-of-concept
  // =========================================================================

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
