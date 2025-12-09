package cps.preprocessor

import org.junit.{Test, Ignore}
import org.junit.Assert._

import scala.util.Success
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.experimental

import cps._
import cps.monads.{*, given}
import cps.testconfig.given
import cps.preprocessor.given

/**
 * Test CpsPreprocessor with compiler plugin (CpsDirect path).
 * Uses the top-level testCpsPreprocessorFuture given from common.
 */
@experimental
class TestCpsPreprocessorDirect:

  // Direct style function that should be preprocessed
  def compute(x: Int)(using CpsDirect[Future]): Int =
    val a = x + 10
    val b = await(Future.successful(20))
    a + b

  @Test
  def testPreprocessorCalledWithDirectStyle(): Unit =
    PreprocessorTracker.reset()

    val f = async[Future] {
      compute(5)
    }

    val result = Await.result(f, 10.seconds)
    assertEquals(35, result)
    assertTrue("Preprocessor should have been called", PreprocessorTracker.wasCalled)


@experimental
object TestCpsPreprocessorDirect:

  def main(args: Array[String]): Unit =
    val test = new TestCpsPreprocessorDirect()
    test.testPreprocessorCalledWithDirectStyle()
    println("Ok")
