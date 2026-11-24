package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** An async lambda passed to a non-inline parameter of a `transparent inline` method: the inliner binds it to a proxy val
  * (https://github.com/dotty-cps-async/dotty-cps-async/issues/174).
  */
object TestInlineProxyLambda {

  class Node(val v: Int) {
    class Child(val w: Int)
    def children: (Child, Child) = (Child(v), Child(v + 1))
  }

  transparent inline def passOn(g: Int => Int): List[Int] = List(1, 2).map(g)

  transparent inline def twice(g: Int => Int): Int = g(1) + g(2)

  transparent inline def applyTwo(g: (Int, Int) => Int): Int = g(1, 2) * 10

  transparent inline def nestedValue(g: Int => Int): List[List[Int]] = List(1, 2).map(x => List(x, x + 1).map(g))

  transparent inline def nestedApply(g: Int => Int): List[Int] = List(1, 2).map(x => g(x) + 1)

  transparent inline def wider(g: Int => Any): List[Any] = List(1, 2).map(g) :+ g(3)

  transparent inline def applyAll(ns: List[Node], g: (n: Node) => n.Child): List[Int] = ns.map(n => g(n).w)

  transparent inline def mapAll(ns: List[Node], g: (n: Node) => n.Child): List[Node#Child] = ns.map(g)

}

class TestInlineProxyLambda {

  import TestInlineProxyLambda.*

  def f(a: Int, b: Int): Future[Int] = Future.successful(a + b)

  @Test def testProxyAsFunctionValue() = {
    val c = async[Future] {
      val r = passOn(x => await(f(x, 10)))
      assert(r == List(11, 12), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testProxyApply() = {
    val c = async[Future] {
      val r = twice(x => await(f(x, 10)))
      assert(r == 23, s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testProxyApplyTwoParams() = {
    val c = async[Future] {
      val r = applyTwo((a, b) => await(f(a, b)))
      assert(r == 30, s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testProxyAsFunctionValueInNestedLambda() = {
    val c = async[Future] {
      val r = nestedValue(x => await(f(x, 100)))
      assert(r == List(List(101, 102), List(102, 103)), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testInlineCallInLambda() = {
    val c = async[Future] {
      val r = List(10, 20).map(y => nestedApply(x => await(f(x, y))))
      assert(r == List(List(12, 13), List(22, 23)), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testDeclaredResultTypeWider() = {
    val c = async[Future] {
      val r = wider(x => await(f(x, 1000)))
      assert(r == List(1001, 1002, 1003), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testDependentFunctionTypeApply() = {
    val c = async[Future] {
      val r = applyAll(List(Node(1), Node(10)), (n: Node) => { await(f(1, 2)); n.children._1 })
      assert(r == List(1, 10), s"r=$r")
    }
    FutureCompleter(c)
  }

  @Test def testDependentFunctionTypeAsFunctionValue() = {
    val c = async[Future] {
      val r = mapAll(List(Node(1), Node(10)), (n: Node) => { await(f(1, 2)); n.children._2 })
      assert(r.map(_.w) == List(2, 11), s"r=$r")
    }
    FutureCompleter(c)
  }

}
