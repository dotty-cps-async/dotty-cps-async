//> using scala 3.9.0
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.4.1-SNAPSHOT
//> using options -Xcheck-macros
// see: https://github.com/dotty-cps-async/dotty-cps-async/issues/170
// Sync arguments which are not pure paths are bound to vals before an async argument. The trees are checked with -Ycheck:all
// (added by cc.TestsCli): the val of a lazy val or an object argument, or of a method with a singleton result type keeps its
// singleton type, on which the types of the next parameters depend; the plugin binds a sync argument with a block, not with
// `flatMap(pure(...))`. A selection from a val (an argument, the qualifier of a call or of an assignment) stays after the async
// part, the val is checked for null before it. The `StringContext` of `f` is
// not bound, and the val of an async argument has its type (not the parameter type `Any`).

import cps.*
import cps.monads.{*, given}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

trait Dep:
  type T
  def mk(n: Int): T
  def get(t: T): Int

// with initialization code: bound before an async argument
object DepInitObject extends Dep:
  val created: Boolean = true
  type T = Int
  def mk(n: Int): Int = n
  def get(t: Int): Int = t

def depPair(x: Dep)(y: x.T, z: Int): Int = x.get(y) * 10 + z

def pair(a: Int, b: Int): Int = a * 10 + b

def curried(a: Int)(b: Int): Int = a * 10 + b

case class Tagged(n: Int)(val tag: Int)

def tagged(t: Tagged, b: Int): Int = t.n * 10 + b

class Box(val v: Int):
  var n: Int = 0
  def plus(x: Int): Int = v + x

class BoxHolder(val box: Box)

@main def test(): Unit =
  lazy val la: Dep = DepInitObject
  val dep: Dep = DepInitObject
  def theDep: dep.type = dep
  def first(): Int = 1
  val boxes = BoxHolder(Box(4))
  val f = async[Future] {
    var x = 1
    val r1 = depPair(la)(la.mk(3), await(Future.successful(4)))
    val r2 = depPair(DepInitObject)(DepInitObject.mk(5), await(Future.successful(6)))
    val r3 = pair(x, await({ x = 2; Future.successful(7) }))
    val r4 = curried(first())(await(Future.successful(8)))
    val r5 = tagged(Tagged(first())(2), await(Future.successful(9)))
    val r6 = List(2, 3).foldLeft(first())((s, e) => s + await(Future.successful(e)))
    // the `StringContext` stays a literal, the argument keeps its type
    val r7 = f"x${await(Future.successful(2))}%03d"
    val r8 = depPair(theDep)(dep.mk(2), await(Future.successful(3)))
    val r9 = pair(boxes.box.v, await(Future.successful(5)))
    val r10 = boxes.box.plus(await(Future.successful(6)))
    boxes.box.n = await(Future.successful(7))
    List(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, boxes.box.n)
  }
  println(Await.result(f, 10.seconds))
