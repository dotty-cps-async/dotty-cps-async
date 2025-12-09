package cps.preprocessor

import org.junit.{Test, Ignore}
import org.junit.Assert._

import scala.util.Success
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import cps._
import cps.monads.{*, given}
import cps.testconfig.given
import cps.preprocessor.given

/**
 * Test CpsPreprocessor with compiler plugin (async[F] path).
 * Uses the top-level testCpsPreprocessorFuture given from common.
 */
class TestCpsPreprocessorPlugin:

  @Test
  def testPreprocessorCalledWithPlugin(): Unit =
    PreprocessorTracker.reset()

    val f = async[Future] {
      val a = 10
      val b = 20
      a + b
    }

    val result = Await.result(f, 10.seconds)
    assertEquals(30, result)
    assertTrue("Preprocessor should have been called", PreprocessorTracker.wasCalled)


object TestCpsPreprocessorPlugin:

  def main(args: Array[String]): Unit =
    val test = new TestCpsPreprocessorPlugin()
    test.testPreprocessorCalledWithPlugin()
    println("Ok")
