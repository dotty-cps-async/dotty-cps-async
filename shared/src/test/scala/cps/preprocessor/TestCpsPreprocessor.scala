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
   * Test that preprocessor can access the context.
   * This verifies that ctx is properly passed to the preprocessor macro.
   */
  @Test def testPreprocessorUsesContext(): Unit =
    import TestPreprocessorWithContext.given

    TestPreprocessorWithContext.reset()

    val c = async[ComputationBound] {
      val x = 42
      x + 1
    }
    val r = c.run()
    assertEquals(Success(43), r)
    // Check that context was accessed
    assertTrue("Context should have been accessed", TestPreprocessorWithContext.contextWasAccessed)

  /**
   * Test that preprocessor is called when using reify/reflect syntax.
   * This verifies that CpsPreprocessor works with the reflect[F] extension.
   */
  @Test def testPreprocessorWithReifyReflect(): Unit =
    import TestPreprocessorTracking.given

    TestPreprocessorTracking.reset()

    val c = reify[ComputationBound] {
      val a = T1.cbi(5).reflect
      val b = T1.cbi(10).reflect
      a + b
    }
    val r = c.run()
    assertEquals(Success(15), r)
    assertTrue("Preprocessor should have been called with reify/reflect", TestPreprocessorTracking.wasCalled)

  /**
   * Test that preprocessor can wrap vals when using reify/reflect syntax.
   */
  @Test def testPreprocessorWrapsValsWithReifyReflect(): Unit =
    import TestPreprocessorWrapping.given

    TestPreprocessorWrapping.reset()

    val c = reify[ComputationBound] {
      val x = T1.cbi(100).reflect
      val y = T1.cbi(200).reflect
      x + y
    }
    val r = c.run()
    assertEquals(Success(300), r)
    assertTrue("Val wrapping counter should be positive with reify/reflect", TestPreprocessorWrapping.wrapCount > 0)

  /**
   * Test that preprocessor can access context when using reify/reflect syntax.
   */
  @Test def testPreprocessorUsesContextWithReifyReflect(): Unit =
    import TestPreprocessorWithContext.given

    TestPreprocessorWithContext.reset()

    val c = reify[ComputationBound] {
      val x = T1.cbi(42).reflect
      x + 1
    }
    val r = c.run()
    assertEquals(Success(43), r)
    assertTrue("Context should have been accessed with reify/reflect", TestPreprocessorWithContext.contextWasAccessed)

  /**
   * Test that preprocessor can insert await calls that are then processed by CPS transform.
   * This is the key test for durable monad use case.
   */
  @Test def testPreprocessorInsertsAwait(): Unit =
    import TestPreprocessorWithAwaitInsertion.given
    import cps.macros.flags.PrintCode
    given PrintCode = PrintCode

    val c = async[ComputationBound] {
      val x = 10
      val y = 20
      x + y
    }
    val r = c.run()
    assertEquals(Success(30), r)


/**
 * Simple preprocessor that just tracks whether it was called.
 * Macro implementation is in TestPreprocessorMacros.scala.
 */
object TestPreprocessorTracking:
  @volatile var wasCalled: Boolean = false

  def reset(): Unit =
    wasCalled = false

  given [C <: CpsMonadContext[ComputationBound]]: CpsPreprocessor[ComputationBound, C] with
    transparent inline def preprocess[A](inline body: A, inline ctx: C): A =
      ${ TestPreprocessorMacros.trackingImpl[A, C]('body, 'ctx) }


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

  given [C <: CpsMonadContext[ComputationBound]]: CpsPreprocessor[ComputationBound, C] with
    transparent inline def preprocess[A](inline body: A, inline ctx: C): A =
      ${ TestPreprocessorMacros.wrappingImpl[A, C]('body, 'ctx) }


/**
 * Preprocessor that demonstrates using the context parameter.
 * This tests that ctx is actually accessible in the macro.
 */
object TestPreprocessorWithContext:
  @volatile var contextWasAccessed: Boolean = false

  def reset(): Unit =
    contextWasAccessed = false

  def recordContextAccess[C <: CpsMonadContext[ComputationBound]](ctx: C): Unit =
    // Access the monad from context to verify it's a real context
    val _ = ctx.monad
    contextWasAccessed = true

  given [C <: CpsMonadContext[ComputationBound]]: CpsPreprocessor[ComputationBound, C] with
    transparent inline def preprocess[A](inline body: A, inline ctx: C): A =
      ${ TestPreprocessorMacros.withContextImpl[A, C]('body, 'ctx) }


/**
 * Preprocessor that inserts await calls around val definitions.
 * This tests that preprocessor-generated await calls are handled by CPS transform.
 */
object TestPreprocessorWithAwaitInsertion:

  given [C <: CpsMonadContext[ComputationBound]]: CpsPreprocessor[ComputationBound, C] with
    transparent inline def preprocess[A](inline body: A, inline ctx: C): A =
      ${ TestPreprocessorMacros.withAwaitInsertionImpl[A, C]('body, 'ctx) }
