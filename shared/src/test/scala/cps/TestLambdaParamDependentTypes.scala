package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** A rebuilt lambda with local definitions and patterns whose types depend on the lambda parameter
  * (https://github.com/dotty-cps-async/dotty-cps-async/issues/150).
  */
object TestLambdaParamDependentTypes {

  class Node(val v: Int) {
    class Child(val w: Int)
    def children: (Child, Child) = (Child(v), Child(v + 1))
    def childList: List[Child] = List(Child(v), Child(v + 1))
    def childOpt: Option[Child] = if (v > 5) Some(Child(v)) else None
  }

  val nodes: List[Node] = List(Node(1), Node(10))

}

class TestLambdaParamDependentTypes {

  import TestLambdaParamDependentTypes.*

  def f(a: Int, b: Int): Future[Int] = Future.successful(a + b)

  @Test def testVals() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        val c = n.children
        val l = c._1
        val r = c._2
        await(f(l.w, r.w))
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testTuplePatternVal() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        val (l, r) = n.children
        await(f(l.w, r.w))
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testTuplePatternValWithoutAwaitInLambda() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        val (l, r) = n.children
        l.w + r.w
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testMatch() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        n.children match {
          case (l, r) =>
            val z: n.Child = l
            await(f(z.w, r.w))
        }
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testOptionMatch() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        n.childOpt match {
          case Some(c) => await(f(c.w, 0))
          case None    => await(f(-1, 0))
        }
      }
      assert(r == List(-1, 10), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testNestedLambda() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        val s = n.childList.map(c => c.w).sum
        await(f(s, 0))
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testLocalDefAndClass() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        def first(): n.Child = n.children._1
        class Wrap(val c: n.Child)
        val w = Wrap(n.children._2)
        await(f(first().w, w.c.w))
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testImportedMembers() = {
    val c = async[Future] {
      val r = nodes.map { (n: Node) =>
        import n.*
        val (l, r) = children
        await(f(l.w, r.w))
      }
      assert(r == List(3, 21), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testPartialFunction() = {
    val c = async[Future] {
      val r = nodes.collect {
        case n if n.v > 5 =>
          val (l, r) = n.children
          await(f(l.w, r.w))
      }
      assert(r == List(21), s"r=$r")
    }
    FutureCompleter(c)
  }

}
