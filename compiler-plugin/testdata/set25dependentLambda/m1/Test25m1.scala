package cpstest

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.given

/** Lambdas with local definitions, patterns and results whose types depend on the lambda parameter
  * (https://github.com/dotty-cps-async/dotty-cps-async/issues/150).
  */
class Node(val v: Int) {
  class Child(val w: Int)
  def children: (Child, Child) = (Child(v), Child(v + 1))
}

@experimental
object Test25m1 {

  val nodes = List(Node(1), Node(10))

  def f(a: Int, b: Int): Future[Int] = Future.successful(a + b)

  def tuplePatternVal()(using CpsDirect[Future]): List[Int] =
    nodes.map { (n: Node) =>
      val (l, r) = n.children
      await(f(l.w, r.w))
    }

  def matchWithTypedVal()(using CpsDirect[Future]): List[Int] =
    nodes.map { (n: Node) =>
      n.children match {
        case (l, r) =>
          val z: n.Child = l
          await(f(z.w, r.w))
      }
    }

  def dependentResult()(using CpsDirect[Future]): List[Int] =
    nodes.map { (n: Node) =>
      await(f(1, 2))
      n.children._1
    }.map(_.w)

  def dependentResultInAsync(): Future[List[Int]] = async[Future] {
    val z = await(f(0, 0))
    nodes.map { (n: Node) =>
      val (l, r) = n.children
      if (z > 0) l else r
    }.map(_.w)
  }

  def main(args: Array[String]): Unit = {
    val r1 = Await.result(async[Future] { tuplePatternVal() }, 1.second)
    val r2 = Await.result(async[Future] { matchWithTypedVal() }, 1.second)
    val r3 = Await.result(async[Future] { dependentResult() }, 1.second)
    val r4 = Await.result(dependentResultInAsync(), 1.second)
    println(s"$r1 $r2 $r3 $r4")
  }

}
