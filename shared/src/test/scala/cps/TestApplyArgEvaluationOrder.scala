package cps

import org.junit.Test

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Method call `f(a, await(...))`: a sync argument `a` is evaluated before a later async argument, as in Scala, and once. Only a
  * pure path (a val, `this`, a literal) stays in the call. See https://github.com/dotty-cps-async/dotty-cps-async/issues/170,
  * https://github.com/dotty-cps-async/dotty-cps-async/issues/171 and, for the qualifier, [[TestApplyQualifierEvaluationOrder]].
  */
class TestApplyArgEvaluationOrder {

  import TestApplyArgEvaluationOrder.*

  // a variable, changed by the async argument: the old value is passed
  @Test def testVarArg() = {
    val c = async[Future] {
      var x = 1
      val r = pair(x, await({ x = 2; Future.successful(0) }))
      assert(r == (1, 0), s"r=$r")
      assert(x == 2, s"x=$x")
    }
    FutureCompleter(c)
  }

  // a parameterless def
  @Test def testParameterlessDefArg() = {
    val log = ArrayBuffer[String]()
    def first: Int = { log += "first"; 1 }
    val c = async[Future] {
      val r = pair(first, await({ log += "second"; Future.successful(0) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a method call: evaluated once (https://github.com/dotty-cps-async/dotty-cps-async/issues/171)
  @Test def testMethodCallArg() = {
    val log = ArrayBuffer[String]()
    def first(): Int = { log += "first"; 1 }
    val c = async[Future] {
      val r = pair(first(), await({ log += "second"; Future.successful(0) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a var field of a val, changed by the async argument
  @Test def testVarFieldArg() = {
    val c = async[Future] {
      val h = new Holder
      val r = pair(h.n, await({ h.n = 2; Future.successful(0) }))
      assert(r == (1, 0), s"r=$r")
      assert(h.n == 2, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // a lazy val: its initializer runs before the async argument
  @Test def testLazyValArg() = {
    val log = ArrayBuffer[String]()
    lazy val first: Int = { log += "first"; 1 }
    val c = async[Future] {
      val r = pair(first, await({ log += "second"; Future.successful(0) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a by-name parameter of the enclosing method
  @Test def testByNameParamArg() = {
    val log = ArrayBuffer[String]()
    def run(first: => Int): Future[(Int, Int)] = async[Future] {
      pair(first, await({ log += "second"; Future.successful(0) }))
    }
    val c = async[Future] {
      val r = await(run({ log += "first"; 1 }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a by-name parameter of a method with an inferred result type, which is the async block
  @Test def testByNameParamArgInferredResult() = {
    val log = ArrayBuffer[String]()
    def run(first: => Int) = async[Future] {
      pair(first, await({ log += "second"; Future.successful(0) }))
    }
    val c = async[Future] {
      val r = await(run({ log += "first"; 1 }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a parameter of a lambda, in which the async block is expanded (the lambda's type is not known yet)
  @Test def testLambdaParamArg() = {
    val c = Future.sequence((1 to 2).map { i =>
      async[Future] {
        pair(i, await(Future.successful(0)))
      }
    })
    FutureCompleter(c.map { r =>
      assert(r == Seq((1, 0), (2, 0)), s"r=$r")
    })
  }

  // the async argument is in the next argument list
  @Test def testCurriedArg() = {
    val c = async[Future] {
      var x = 1
      val r = curried(x)(await({ x = 2; Future.successful(0) }))
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // elements of varargs
  @Test def testVarargsArg() = {
    val log = ArrayBuffer[String]()
    def first(): Int = { log += "first"; 1 }
    val c = async[Future] {
      var x = 1
      val r = List(x, first(), await({ x = 2; log += "second"; Future.successful(0) }))
      assert(r == List(1, 1, 0), s"r=$r")
      assert(log.toList == List("first", "second"), s"log=$log")
    }
    FutureCompleter(c)
  }

  // a sync argument after the async one sees its effects
  @Test def testArgAfterAsync() = {
    val c = async[Future] {
      var x = 1
      val r = pair(await({ x = 2; Future.successful(0) }), x)
      assert(r == (0, 2), s"r=$r")
    }
    FutureCompleter(c)
  }

  // vals, a literal, `this`, a spliced val (they stay in the call) and a case class instance: only the results are checked
  @Test def testPurePathArgs() = {
    val c = async[Future] {
      val h = new Holder
      val y = 3
      val r = h.triple(y, 4, await(Future.successful(5)))
      assert(r == (3, 4, 5), s"r=$r")
      val s = await(h.withThis())
      assert(s == (h, 6), s"s=$s")
      val xs = Seq(1, 2)
      val v = curriedVarargs(xs*)(await(Future.successful(7)))
      assert(v == (Seq(1, 2), 7), s"v=$v")
      val p = point(Point(y, 2), await(Future.successful(8)))
      assert(p == (3, 2, 8), s"p=$p")
    }
    FutureCompleter(c)
  }

  // a curried case class with a method call in its first argument list
  @Test def testCurriedCaseClassArg() = {
    val log = ArrayBuffer[String]()
    def first(): Int = { log += "first"; 1 }
    val c = async[Future] {
      val r = tagged(Tagged(first())("t"), await({ log += "second"; Future.successful(0) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, "t", 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a case class with initialization code: its constructor runs before the async argument
  @Test def testCaseClassWithInitArg() = {
    val log = ArrayBuffer[String]()
    val c = async[Future] {
      val r = logged(Logged(1, log), await({ log += "second"; Future.successful(0) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a case class, which companion object has initialization code: the object is initialized before the async argument
  @Test def testCaseClassCompanionWithInitArg() = {
    val c = async[Future] {
      val r = initCompanion(InitCompanion(1), await({ companionLog += "second"; Future.successful(0) }))
      assert(companionLog.toList == List("init", "second"), s"log=$companionLog")
      assert(r == (1, 0), s"r=$r")
    }
    FutureCompleter(c)
  }

  // a case class of an outer instance, which is a variable changed by the async argument: the old instance is used
  @Test def testCaseClassWithImpureOuterArg() = {
    val c = async[Future] {
      var outer = new Outer(1)
      val r = innerArg(outer.Inner(5), await({ outer = new Outer(2); Future.successful(3) }))
      assert(r == (1, 5, 3), s"r=$r")
    }
    FutureCompleter(c)
  }

  // the types of the next parameters depend on a lazy val or an object argument
  @Test def testDependentParamArgs() = {
    lazy val la: Dep = DepObject
    val c = async[Future] {
      val r1 = depPair(la)(la.mk(3), await(Future.successful(4)))
      assert(r1 == (3, 4), s"r1=$r1")
      val r2 = depPair(DepObject)(DepObject.mk(5), await(Future.successful(6)))
      assert(r2 == (5, 6), s"r2=$r2")
      // an object with initialization code is bound
      val r3 = depPair(DepInitObject)(DepInitObject.mk(7), await(Future.successful(8)))
      assert(r3 == (7, 8), s"r3=$r3")
    }
    FutureCompleter(c)
  }

  // the value of an argument is a stable path, the types of the next parameters depend on it: the result of a parameterless
  // method, of a method call and of a parameterless method imported from an object
  @Test def testSingletonResultDependentParamArgs() = {
    val log = ArrayBuffer[String]()
    val dep: Dep = DepObject
    def theDep: dep.type = { log += "first"; dep }
    def theDepCall(): dep.type = { log += "first"; dep }
    val c = async[Future] {
      val r1 = depPair(theDep)(dep.mk(1), await({ log += "second"; Future.successful(2) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r1 == (1, 2), s"r1=$r1")
      log.clear()
      val r2 = depPair(theDepCall())(dep.mk(3), await({ log += "second"; Future.successful(4) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r2 == (3, 4), s"r2=$r2")
      val r3 = depPair(sharedDepDef)(sharedDep.mk(5), await(Future.successful(6)))
      assert(r3 == (5, 6), s"r3=$r3")
    }
    FutureCompleter(c)
  }

  // a case class with an argument imported from an object with initialization code: the object is initialized before the async
  // argument
  @Test def testImportedValInCaseClassArg() = {
    import ImportedInitFields.n
    val c = async[Future] {
      val r = point(Point(n, 2), await({ importedFieldsLog += "second"; Future.successful(3) }))
      assert(importedFieldsLog.toList == List("init", "second"), s"log=$importedFieldsLog")
      assert(r == (1, 2, 3), s"r=$r")
    }
    FutureCompleter(c)
  }

  // an ascribed val imported from an object with initialization code: the object is initialized before the async argument
  @Test def testImportedValInTypedArg() = {
    import ImportedTypedInitFields.n
    val c = async[Future] {
      val r = pair(n: Int, await({ importedTypedFieldsLog += "second"; Future.successful(2) }))
      assert(importedTypedFieldsLog.toList == List("init", "second"), s"log=$importedTypedFieldsLog")
      assert(r == (1, 2), s"r=$r")
    }
    FutureCompleter(c)
  }

  // new instances before an async argument in a monad, which runs the rest of the computation for each element (`List`): each
  // instance is created once, as in Scala
  @Test def testNewInstanceArgInListMonad() = {
    val cells = reify[List] { first(MultiShotCell(0), reflect(List(1, 2))) }
    assert(cells.length == 2 && (cells(0) eq cells(1)), s"cells=$cells")
    val holders = reify[List] { first(new Holder, reflect(List(1, 2))) }
    assert(holders.length == 2 && (holders(0) eq holders(1)), s"holders=$holders")
  }

  // the call with a sync argument and a shifted lambda (no async argument): the argument is evaluated once, after the qualifier
  @Test def testMethodCallArgWithShiftedLambda() = {
    val log = ArrayBuffer[String]()
    def qualifier(): List[Int] = { log += "qualifier"; List(2, 3) }
    def first(): Int = { log += "first"; 1 }
    val c = async[Future] {
      val r = qualifier().foldLeft(first())((s, e) => s + await(Future.successful(e)))
      assert(log.toList == List("qualifier", "first"), s"log=$log")
      assert(r == 6, s"r=$r")
    }
    FutureCompleter(c)
  }

  // a variable, changed by the qualifier, in a call with a shifted lambda: the new value is passed
  @Test def testVarArgChangedByQualifierWithShiftedLambda() = {
    val c = async[Future] {
      var counter = 0
      def qualifier(): List[Int] = { counter = 10; List(1, 2) }
      val r = qualifier().foldLeft(counter)((s, e) => s + await(Future.successful(e)))
      assert(r == 13, s"r=$r")
    }
    FutureCompleter(c)
  }

}

object TestApplyArgEvaluationOrder {

  def pair(a: Int, b: Int): (Int, Int) = (a, b)

  def curried(a: Int)(b: Int): (Int, Int) = (a, b)

  def curriedVarargs(xs: Int*)(b: Int): (Seq[Int], Int) = (xs, b)

  case class Point(x: Int, y: Int)

  def point(p: Point, b: Int): (Int, Int, Int) = (p.x, p.y, b)

  case class Tagged(n: Int)(val tag: String)

  def tagged(t: Tagged, b: Int): (Int, String, Int) = (t.n, t.tag, b)

  case class Logged(n: Int, log: ArrayBuffer[String]) {
    log += "first"
  }

  def logged(l: Logged, b: Int): (Int, Int) = (l.n, b)

  trait Dep {
    type T
    def mk(n: Int): T
    def get(t: T): Int
  }

  object DepObject extends Dep {
    type T = Int
    def mk(n: Int): Int = n
    def get(t: Int): Int = t
  }

  object DepInitObject extends Dep {
    val created: Boolean = true
    type T = Int
    def mk(n: Int): Int = n
    def get(t: Int): Int = t
  }

  def depPair(x: Dep)(y: x.T, z: Int): (Int, Int) = (x.get(y), z)

  val sharedDep: Dep = DepObject

  def sharedDepDef: sharedDep.type = sharedDep

  def first[A](a: A, b: Int): A = a

  // only `testImportedValInCaseClassArg` touches ImportedInitFields
  val importedFieldsLog = ArrayBuffer[String]()

  object ImportedInitFields {
    importedFieldsLog += "init"
    val n: Int = 1
  }

  // only `testImportedValInTypedArg` touches ImportedTypedInitFields
  val importedTypedFieldsLog = ArrayBuffer[String]()

  object ImportedTypedInitFields {
    importedTypedFieldsLog += "init"
    val n: Int = 1
  }

  // only `testCaseClassCompanionWithInitArg` touches InitCompanion
  val companionLog = ArrayBuffer[String]()

  case class InitCompanion(n: Int)

  object InitCompanion {
    companionLog += "init"
  }

  def initCompanion(i: InitCompanion, b: Int): (Int, Int) = (i.n, b)

  class Outer(val k: Int) {
    case class Inner(n: Int) {
      def outerK: Int = k
    }
  }

  def innerArg(i: Outer#Inner, b: Int): (Int, Int, Int) = (i.outerK, i.n, b)

  class Holder {
    var n: Int = 1
    def triple(a: Int, b: Int, c: Int): (Int, Int, Int) = (a, b, c)
    def withThis(): Future[(Holder, Int)] = async[Future] {
      pairThis(this, await(Future.successful(6)))
    }
    private def pairThis(h: Holder, b: Int): (Holder, Int) = (h, b)
  }

}

// at the top level: its path is pure (a case class in an object with initialization code is bound with the object)
case class MultiShotCell(var v: Int)
