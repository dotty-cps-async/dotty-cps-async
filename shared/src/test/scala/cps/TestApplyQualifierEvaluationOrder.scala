package cps

import org.junit.Test

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Method call `q.m(await(...))`: the qualifier `q` is evaluated before the arguments, as in Scala (before, the arguments were
  * evaluated first: `arg.map(x => q.m(x))`). See https://github.com/dotty-cps-async/dotty-cps-async/issues/169 and, for
  * assignments, [[TestAssignQualifierEvaluationOrder]].
  */
class TestApplyQualifierEvaluationOrder {

  import TestApplyQualifierEvaluationOrder.*

  // the qualifier is a method call
  @Test def testMethodCallQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    val c = async[Future] {
      track().set(await(right()))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // sync arguments before the async one are evaluated after the qualifier
  @Test def testMethodCallQualifierWithSyncArgs() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def left(): Int = { log += "left"; 2 }
    def right(): Future[Int] = { log += "right"; Future.successful(3) }
    val c = async[Future] {
      val r = track().sum(left(), await(right()))
      // `left()` is evaluated once (https://github.com/dotty-cps-async/dotty-cps-async/issues/171)
      assert(log.toList == List("qualifier", "left", "right"), s"log=$log")
      assert(r == 5, s"r=$r")
    }
    FutureCompleter(c)
  }

  // the async argument is in the second argument list
  @Test def testCurriedMethodCallQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def right(): Future[Int] = { log += "right"; Future.successful(3) }
    val c = async[Future] {
      val r = track().curried(2)(await(right()))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(r == 5, s"r=$r")
    }
    FutureCompleter(c)
  }

  // a method with a type parameter
  @Test def testTypeAppliedMethodCallQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def right(): Future[Int] = { log += "right"; Future.successful(4) }
    val c = async[Future] {
      val r = track().pair[Int](await(right()))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(r == (0, 4), s"r=$r")
    }
    FutureCompleter(c)
  }

  // the qualifier is a variable, changed by the argument: the old value is used
  @Test def testVarQualifierChangedByArgument() = {
    val h1 = new Holder
    val h2 = new Holder
    val c = async[Future] {
      var cur = h1
      cur.set(await({ cur = h2; Future.successful(1) }))
      assert(h1.n == 1, s"h1.n=${h1.n}")
      assert(h2.n == 0, s"h2.n=${h2.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a parameter of a lambda, in which the async block is expanded (the lambda's type is not known yet)
  @Test def testLambdaParamQualifier() = {
    val hs = List(new Holder, new Holder)
    val c = Future.sequence(hs.map { h =>
      async[Future] {
        h.set(await(Future.successful(5)))
      }
    })
    FutureCompleter(c.map { _ =>
      assert(hs.map(_.n) == List(5, 5), s"hs=${hs.map(_.n)}")
    })
  }

  // the qualifier is a parameter of a method with an inferred result type, which is the async block
  @Test def testInferredResultMethodParamQualifier() = {
    def run(h: Holder) = async[Future] {
      h.set(await(Future.successful(3)))
    }
    val h = new Holder
    FutureCompleter(run(h).map { _ =>
      assert(h.n == 3, s"h.n=${h.n}")
    })
  }

  // compound assignment `x += await(...)` is `x = x.+(await(...))`: the old value of `x` is used
  @Test def testCompoundAssignment() = {
    val c = async[Future] {
      var x = 1
      x += await({ x = 100; Future.successful(1) })
      assert(x == 2, s"x=$x")
    }
    FutureCompleter(c)
  }

  // compound assignment to a field `q.x += await(...)`
  @Test def testCompoundAssignmentToField() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    h.n = 1
    def track(): Holder = { log += "qualifier"; h }
    val c = async[Future] {
      track().n += await({ log += "right"; h.n = 100; Future.successful(1) })
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 2, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // indexed update `q.arr(i) = await(...)` is `q.arr.update(i, await(...))`
  @Test def testIndexedUpdate() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def right(): Future[Int] = { log += "right"; Future.successful(7) }
    val c = async[Future] {
      track().arr(1) = await(right())
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.arr.toList == List(0, 7, 0), s"h.arr=${h.arr.toList}")
    }
    FutureCompleter(c)
  }

  // a user-defined setter `q.prop = await(...)` is `q.prop_=(await(...))`
  @Test def testUserDefinedSetter() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def right(): Future[Int] = { log += "right"; Future.successful(8) }
    val c = async[Future] {
      track().prop = await(right())
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 8, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a lazy val: it is initialized before the argument
  @Test def testLazyValQualifier() = {
    val log = ArrayBuffer[String]()
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    val c = async[Future] {
      lazy val h = { log += "qualifier"; new Holder }
      h.set(await(right()))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a lazy val, the parameter and the result types of the method depend on it
  @Test def testLazyValQualifierPathDependentMethod() = {
    val c = async[Future] {
      lazy val b: Box = intBox(7)
      val r: b.T = b.put(await(Future.successful(b.mk)))
      assert(r == b.mk, s"r=$r")
      assert(b.v == 7, s"b.v=${b.v}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a method call with the singleton type of a val, the types of the method depend on it
  @Test def testSingletonTypedMethodCallQualifierPathDependentMethod() = {
    val log = ArrayBuffer[String]()
    val b: Box = intBox(3)
    def theBox(): b.type = { log += "qualifier"; b }
    val c = async[Future] {
      val r: b.T = theBox().put(await({ log += "right"; Future.successful(b.mk) }))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(r == b.mk, s"r=$r")
      assert(b.v == 3, s"b.v=${b.v}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a method call, the result type of the method depends on it
  @Test def testMethodCallQualifierPathDependentResult() = {
    val log = ArrayBuffer[String]()
    def mkBox(): Box = { log += "qualifier"; intBox(2) }
    val c = async[Future] {
      val r = mkBox().at(await({ log += "right"; Future.successful(0) }))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(r == 2, s"r=$r")
    }
    FutureCompleter(c)
  }

  // the qualifier is a method call with a `this.type` result, the types of the method depend on it
  @Test def testThisTypedMethodCallQualifierPathDependentMethod() = {
    val b = intSelfBox(5)
    val c = async[Future] {
      await(b.putSelf())
      assert(b.v == 5, s"b.v=${b.v}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a by-name parameter: its argument is evaluated before the async argument
  @Test def testByNameParamQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    def setTo(q: => Holder): Future[Unit] = async[Future] {
      q.set(await(right()))
    }
    val c = async[Future] {
      await(setTo({ log += "qualifier"; h }))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is an object with an initializer: it is initialized before the argument
  @Test def testObjectQualifierInitialization() = {
    val c = async[Future] {
      InitLoggedObject.set(await({ objectLog += "right"; Future.successful(1) }))
      assert(objectLog.toList == List("init", "right"), s"log=$objectLog")
      assert(InitLoggedObject.n == 1, s"n=${InitLoggedObject.n}")
    }
    FutureCompleter(c)
  }

  // the method is imported from an object with an initializer: the object is initialized before the argument
  @Test def testImportedMethodObjectInitialization() = {
    val c = async[Future] {
      import ImportedInitLoggedObject.*
      set(await({ importedObjectLog += "right"; Future.successful(1) }))
      assert(importedObjectLog.toList == List("init", "right"), s"log=$importedObjectLog")
      assert(ImportedInitLoggedObject.n == 1, s"n=${ImportedInitLoggedObject.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a val imported from an object with an initializer: the object is initialized before the argument
  @Test def testImportedValObjectInitialization() = {
    val c = async[Future] {
      import ImportedValInitLoggedObject.*
      holder.set(await({ importedValObjectLog += "right"; Future.successful(1) }))
      assert(importedValObjectLog.toList == List("init", "right"), s"log=$importedValObjectLog")
      assert(ImportedValInitLoggedObject.holder.n == 1, s"n=${ImportedValInitLoggedObject.holder.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is changed by the transformation (an inner lambda is unshifted), but sync
  @Test def testChangedSyncQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(fs: List[Future[Int]]): Holder = { log += "qualifier"; h.fs = fs; h }
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    val c = async[Future] {
      track(List(1, 2).map(y => { await(Future.successful(y)); Future.successful(y) })).set(await(right()))
      val values = await(Future.sequence(h.fs))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // an async argument and a shifted async lambda: the qualifier is evaluated before the async argument
  @Test def testMethodCallQualifierWithShiftedLambda() = {
    val log = ArrayBuffer[String]()
    def track(): List[Int] = { log += "qualifier"; List(1, 2) }
    def right(): Future[Int] = { log += "right"; Future.successful(10) }
    val c = async[Future] {
      val r = track().foldLeft(await(right()))((s, x) => s + await(Future.successful(x)))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(r == 13, s"r=$r")
    }
    FutureCompleter(c)
  }

  // a constructor call: `new C` is not a qualifier value, it is not bound
  @Test def testConstructorCall() = {
    val c = async[Future] {
      val b = new IntCell(await(Future.successful(3)))
      val g = new Cell[String](await(Future.successful("x")))
      assert(b.x == 3, s"b.x=${b.x}")
      assert(g.x == "x", s"g.x=${g.x}")
    }
    FutureCompleter(c)
  }

  // a local val, a path through vals, `this` (not bound) and an object (bound with its singleton type): only the results are checked
  @Test def testStableQualifiers() = {
    val h = new Holder
    val nested = new Nested(new Holder)
    val c = async[Future] {
      h.set(await(Future.successful(1)))
      nested.holder.set(await(Future.successful(2)))
      HolderObject.set(await(Future.successful(3)))
      await(nested.setThis(4))
      assert(h.n == 1, s"h.n=${h.n}")
      assert(nested.holder.n == 2, s"nested.holder.n=${nested.holder.n}")
      assert(HolderObject.n == 3, s"HolderObject.n=${HolderObject.n}")
      assert(nested.m == 4, s"nested.m=${nested.m}")
    }
    FutureCompleter(c)
  }

  // a case class instance as the qualifier: a method call in the first argument list of a curried case class, initialization code
  @Test def testCaseClassQualifiers() = {
    val log = ArrayBuffer[String]()
    def first(): Int = { log += "first"; 1 }
    val c = async[Future] {
      val r1 = TaggedCell(first())(1).plus(await({ log += "second"; Future.successful(2) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r1 == 4, s"r1=$r1")
      log.clear()
      val r2 = LoggedCell(1, log).plus(await({ log += "second"; Future.successful(2) }))
      assert(log.toList == List("first", "second"), s"log=$log")
      assert(r2 == 3, s"r2=$r2")
    }
    FutureCompleter(c)
  }

}

object TestApplyQualifierEvaluationOrder {

  case class TaggedCell(x: Int)(val y: Int) {
    def plus(z: Int): Int = x + y + z
  }

  case class LoggedCell(x: Int, log: ArrayBuffer[String]) {
    log += "first"
    def plus(z: Int): Int = x + z
  }

  class Holder {
    var n: Int = 0
    var fs: List[Future[Int]] = Nil
    val arr: Array[Int] = Array(0, 0, 0)
    def set(x: Int): Unit = n = x
    def sum(x: Int, y: Int): Int = x + y
    def curried(x: Int)(y: Int): Int = x + y
    def pair[A](x: A): (Int, A) = (n, x)
    def prop: Int = n
    def prop_=(x: Int): Unit = n = x
  }

  class IntCell(val x: Int)

  class Cell[A](val x: A)

  class Nested(val holder: Holder) {
    var m: Int = 0
    def setM(x: Int): Unit = m = x
    def setThis(x: Int): Future[Unit] = async[Future] {
      this.setM(await(Future.successful(x)))
    }
  }

  object HolderObject {
    var n: Int = 0
    def set(x: Int): Unit = n = x
  }

  // only `testObjectQualifierInitialization` touches InitLoggedObject
  val objectLog = ArrayBuffer[String]()

  // only `testImportedMethodObjectInitialization` touches ImportedInitLoggedObject
  val importedObjectLog = ArrayBuffer[String]()

  object InitLoggedObject {
    objectLog += "init"
    var n: Int = 0
    def set(x: Int): Unit = n = x
  }

  object ImportedInitLoggedObject {
    importedObjectLog += "init"
    var n: Int = 0
    def set(x: Int): Unit = n = x
  }

  // only `testImportedValObjectInitialization` touches ImportedValInitLoggedObject
  val importedValObjectLog = ArrayBuffer[String]()

  object ImportedValInitLoggedObject {
    importedValObjectLog += "init"
    val holder = new Holder
  }

  trait Box {
    type T
    var v: T
    def mk: T
    def put(x: T): T = { v = x; x }
    def at(i: Int): T = mk
  }

  def intBox(x: Int): Box = new Box {
    type T = Int
    var v: Int = 0
    def mk: Int = x
  }

  abstract class SelfBox {
    type T
    var v: T
    def mk: T
    def self(): this.type = this
    def put(x: T): T = { v = x; x }
    def putSelf(): Future[Unit] = async[Future] {
      val r: T = self().put(await(Future.successful(mk)))
    }
  }

  def intSelfBox(x: Int): SelfBox = new SelfBox {
    type T = Int
    var v: Int = 0
    def mk: Int = x
  }

}
