package cps.preprocessor

import org.junit.{Test, Ignore}
import org.junit.Assert._

import scala.util.Success
import cps._
import cps.testconfig.given

/**
 * Tests for CpsPreprocessor functionality.
 */
class TestCpsPreprocessor:

  /**
   * Test that async block works without preprocessor (baseline).
   */
  @Test def testNoPreprocessor(): Unit =
    val c = async[ComputationBound] {
      val x = 1
      val y = 2
      x + y
    }
    val r = c.run()
    assertEquals(Success(3), r)

  /**
   * Test that preprocessor is called and can transform the body.
   * Uses a simple preprocessor that tracks val definitions.
   */
  @Test def testPreprocessorCalled(): Unit =
    import TestPreprocessorTracking.given

    TestPreprocessorTracking.reset()

    val c = async[ComputationBound] {
      val a = 10
      val b = 20
      a + b
    }
    val r = c.run()
    assertEquals(Success(30), r)
    // The preprocessor should have been called
    assertTrue("Preprocessor should have been called", TestPreprocessorTracking.wasCalled)

  /**
   * Test preprocessor with await expressions.
   */
  @Test def testPreprocessorWithAwait(): Unit =
    import TestPreprocessorTracking.given

    TestPreprocessorTracking.reset()

    val c = async[ComputationBound] {
      val a = await(T1.cbi(5))
      val b = await(T1.cbi(10))
      a + b
    }
    val r = c.run()
    assertEquals(Success(15), r)
    assertTrue("Preprocessor should have been called", TestPreprocessorTracking.wasCalled)

  /**
   * Test that preprocessor can wrap val definitions.
   */
  @Test def testPreprocessorWrapsVals(): Unit =
    import TestPreprocessorWrapping.given

    TestPreprocessorWrapping.reset()

    val c = async[ComputationBound] {
      val x = 100
      val y = 200
      x + y
    }
    val r = c.run()
    assertEquals(Success(300), r)
    // Check that vals were wrapped (counter should be > 0)
    assertTrue("Val wrapping counter should be positive", TestPreprocessorWrapping.wrapCount > 0)


/**
 * Simple preprocessor that just tracks whether it was called.
 * Macro implementation is in TestPreprocessorMacros.scala.
 */
object TestPreprocessorTracking:
  @volatile var wasCalled: Boolean = false

  def reset(): Unit =
    wasCalled = false

  given CpsPreprocessor[ComputationBound] with
    inline def preprocess[A](inline body: A): A =
      ${ TestPreprocessorMacros.trackingImpl[A]('body) }


/**
 * Preprocessor that wraps expressions with a counter.
 * Macro implementation is in TestPreprocessorMacros.scala.
 */
object TestPreprocessorWrapping:
  @volatile var wrapCount: Int = 0

  def reset(): Unit =
    wrapCount = 0

  def wrap[T](value: T): T =
    wrapCount += 1
    value

  given CpsPreprocessor[ComputationBound] with
    inline def preprocess[A](inline body: A): A =
      ${ TestPreprocessorMacros.wrappingImpl[A]('body) }
