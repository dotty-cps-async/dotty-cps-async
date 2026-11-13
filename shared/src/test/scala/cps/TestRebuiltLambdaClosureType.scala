package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** A lambda rebuilt by the transformation keeps its closure type (a SAM type, a partial function) and its kind (a context
  * function).
  */
object TestRebuiltLambdaClosureType {

  trait IntOp {
    def apply(x: Int): Int
  }

  def withCtx[T](f: Int ?=> T): T = f(using 5)

}

class TestRebuiltLambdaClosureType {

  import TestRebuiltLambdaClosureType.*

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  // the case body is a block, so the match is reported as changed and the lambda is rebuilt
  @Test def testPartialFunctionValWithBlockCase() = {
    val c = async[Future] {
      val pf: PartialFunction[Int, Int] = { case x if x > 1 => { val y = x + 1; y } }
      val z = await(g(1))
      val r = List(1, 2, 3).collect(pf).map(_ + z)
      assert(r == List(13, 14), s"r=$r")
    }
    FutureCompleter(c)
  }

  // the case is changed (the inner lambda is unshifted), but stays sync
  @Test def testPartialFunctionValWithChangedSyncCase() = {
    val c = async[Future] {
      val pf: PartialFunction[Int, List[Future[Int]]] = {
        case x if x > 1 => List(x).map(y => { await(g(y)); Future.successful(y) })
      }
      val values = await(Future.sequence(List(1, 2).collect(pf).flatten))
      assert(values == List(2), s"values=$values")
    }
    FutureCompleter(c)
  }

  @Test def testSamLambdaWithMatchBody() = {
    val c = async[Future] {
      val op: IntOp = x =>
        x match {
          case 1 => { val y = x + 1; y }
          case _ => x
        }
      val z = await(g(1))
      val r = op(1) + op(3) + z
      assert(r == 15, s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testSamLambdaWithBlockBody() = {
    val c = async[Future] {
      val op: IntOp = x => { val y = x + 1; y }
      val z = await(g(1))
      val r = op(1) + z
      assert(r == 12, s"r=$r")
    }
    FutureCompleter(c)
  }

  // the result of the context function is in F, so it is unshifted, and stays a context function
  @Test def testContextFunctionWithResultInF() = {
    val c = async[Future] {
      val r = await(withCtx { val y = await(g(summon[Int])); Future.successful(y) })
      assert(r == 50, s"r=$r")
    }
    FutureCompleter(c)
  }

}
