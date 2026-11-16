//> using scala 3.9.0
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.4.1-SNAPSHOT
//> using options -Xcheck-macros
// see: https://github.com/dotty-cps-async/dotty-cps-async/issues/170
// Sync arguments which are not pure paths are bound to vals before an async argument. The trees are checked with -Ycheck:all
// (added by cc.TestsCli): the val of a lazy val or an object argument keeps its singleton type, on which the types of the next
// parameters depend; the plugin binds a sync argument with a block, not with `flatMap(pure(...))`.

import cps.*
import cps.monads.{*, given}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

trait Dep:
  type T
  def mk(n: Int): T
  def get(t: T): Int

object DepObject extends Dep:
  type T = Int
  def mk(n: Int): Int = n
  def get(t: Int): Int = t

def depPair(x: Dep)(y: x.T, z: Int): Int = x.get(y) * 10 + z

def pair(a: Int, b: Int): Int = a * 10 + b

def curried(a: Int)(b: Int): Int = a * 10 + b

case class Tagged(n: Int)(val tag: Int)

def tagged(t: Tagged, b: Int): Int = t.n * 10 + b

@main def test(): Unit =
  lazy val la: Dep = DepObject
  def first(): Int = 1
  val f = async[Future] {
    var x = 1
    val r1 = depPair(la)(la.mk(3), await(Future.successful(4)))
    val r2 = depPair(DepObject)(DepObject.mk(5), await(Future.successful(6)))
    val r3 = pair(x, await({ x = 2; Future.successful(7) }))
    val r4 = curried(first())(await(Future.successful(8)))
    val r5 = tagged(Tagged(first())(2), await(Future.successful(9)))
    val r6 = List(2, 3).foldLeft(first())((s, e) => s + await(Future.successful(e)))
    List(r1, r2, r3, r4, r5, r6)
  }
  println(Await.result(f, 10.seconds))
