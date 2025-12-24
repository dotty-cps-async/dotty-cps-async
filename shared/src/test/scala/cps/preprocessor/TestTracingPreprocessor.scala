package cps.preprocessor

import org.junit.{Test, Before}
import org.junit.Assert.*

import scala.util.Success
import cps.*
import cps.testconfig.given

/**
 * Tests for the TracingPreprocessor example.
 * Demonstrates CpsPreprocessor usage for documentation.
 */
class TestTracingPreprocessor:

  @Before def setup(): Unit =
    TracingPreprocessor.clearLog()

  @Test def testTracingSimpleVals(): Unit =
    import TracingPreprocessor.given

    val c = async[ComputationBound] {
      val x = 1 + 1
      val y = x * 2
      y + 10
    }

    val r = c.run()
    assertEquals(Success(14), r)

    val log = TracingPreprocessor.getLog
    assertEquals(2, log.size)
    assertEquals("x = 2", log(0))
    assertEquals("y = 4", log(1))

  @Test def testTracingWithAwait(): Unit =
    import TracingPreprocessor.given

    val c = async[ComputationBound] {
      val a = await(T1.cbi(10))
      val b = await(T1.cbi(20))
      a + b
    }

    val r = c.run()
    assertEquals(Success(30), r)

    val log = TracingPreprocessor.getLog
    assertEquals(2, log.size)
    assertEquals("a = 10", log(0))
    assertEquals("b = 20", log(1))

  @Test def testTracingWithStringVals(): Unit =
    import TracingPreprocessor.given

    val c = async[ComputationBound] {
      val greeting = "Hello"
      val name = "World"
      s"$greeting, $name!"
    }

    val r = c.run()
    assertEquals(Success("Hello, World!"), r)

    val log = TracingPreprocessor.getLog
    assertEquals(2, log.size)
    assertEquals("greeting = Hello", log(0))
    assertEquals("name = World", log(1))

  @Test def testTracingWithReifyReflect(): Unit =
    import TracingPreprocessor.given

    val c = reify[ComputationBound] {
      val x = T1.cbi(5).reflect
      val y = T1.cbi(7).reflect
      x * y
    }

    val r = c.run()
    assertEquals(Success(35), r)

    val log = TracingPreprocessor.getLog
    assertEquals(2, log.size)
    assertEquals("x = 5", log(0))
    assertEquals("y = 7", log(1))
