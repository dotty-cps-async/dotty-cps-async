package cps

import org.junit.Test

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** Assignment `q.f = await(...)`: the qualifier `q` is evaluated before the right part, as in Scala (before, the right part was evaluated
  * first: `rhs.map(x => q.f = x)`). See https://github.com/dotty-cps-async/dotty-cps-async/issues/165
  */
class TestAssignQualifierEvaluationOrder {

  import TestAssignQualifierEvaluationOrder.*

  // the qualifier is a method call
  @Test def testMethodCallQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def track(): Holder = { log += "qualifier"; h }
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    val c = async[Future] {
      track().n = await(right())
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a variable, changed by the right part: the old value is used
  @Test def testVarQualifierChangedByRight() = {
    val h1 = new Holder
    val h2 = new Holder
    val c = async[Future] {
      var cur = h1
      cur.n = await({ cur = h2; Future.successful(1) })
      assert(h1.n == 1, s"h1.n=${h1.n}")
      assert(h2.n == 0, s"h2.n=${h2.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a lazy val: it is initialized before the right part
  @Test def testLazyValQualifier() = {
    val log = ArrayBuffer[String]()
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    val c = async[Future] {
      lazy val h = { log += "qualifier"; new Holder }
      h.n = await(right())
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a by-name parameter: its argument is evaluated before the right part
  @Test def testByNameParamQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    def right(): Future[Int] = { log += "right"; Future.successful(1) }
    def assignTo(q: => Holder): Future[Unit] = async[Future] {
      q.n = await(right())
    }
    val c = async[Future] {
      await(assignTo({ log += "qualifier"; h }))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a by-name class parameter
  @Test def testByNameClassParamQualifier() = {
    val log = ArrayBuffer[String]()
    val h = new Holder
    val ref = new ByNameRef({ log += "qualifier"; h }, log)
    val c = async[Future] {
      await(ref.assign(1))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a lazy val, the type of the field depends on it
  @Test def testLazyValQualifierPathDependentField() = {
    val c = async[Future] {
      lazy val b: Box = intBox(7)
      b.v = await(Future.successful(b.mk))
      assert(b.v == 7, s"b.v=${b.v}")
    }
    FutureCompleter(c)
  }

  // the qualifier is an object with an initializer: it is initialized before the right part
  @Test def testObjectQualifierInitialization() = {
    val c = async[Future] {
      InitLoggedObject.n = await({ objectLog += "right"; Future.successful(1) })
      assert(objectLog.toList == List("init", "right"), s"log=$objectLog")
      assert(InitLoggedObject.n == 1, s"n=${InitLoggedObject.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a method call with a `this.type` result, the type of the field depends on it
  @Test def testThisTypedMethodCallQualifierPathDependentField() = {
    val b = intSelfBox(5)
    val c = async[Future] {
      await(b.assignSelf())
      assert(b.v == 5, s"b.v=${b.v}")
    }
    FutureCompleter(c)
  }

  // the left part is a member imported from an object with an initializer: the object is initialized before the right part
  @Test def testImportedMemberObjectInitialization() = {
    val c = async[Future] {
      import ImportedInitLoggedObject.*
      n = await({ importedObjectLog += "right"; Future.successful(1) })
      assert(importedObjectLog.toList == List("init", "right"), s"log=$importedObjectLog")
      assert(ImportedInitLoggedObject.n == 1, s"n=${ImportedInitLoggedObject.n}")
    }
    FutureCompleter(c)
  }

  // the qualifier is a method call with the singleton type of a val, the type of the field depends on it
  @Test def testSingletonTypedMethodCallQualifierPathDependentField() = {
    val log = ArrayBuffer[String]()
    val b: Box = intBox(3)
    def theBox(): b.type = { log += "qualifier"; b }
    val c = async[Future] {
      theBox().v = await({ log += "right"; Future.successful(b.mk) })
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(b.v == 3, s"b.v=${b.v}")
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
      track(List(1, 2).map(y => { await(Future.successful(y)); Future.successful(y) })).n = await(right())
      val values = await(Future.sequence(h.fs))
      assert(log.toList == List("qualifier", "right"), s"log=$log")
      assert(h.n == 1, s"h.n=${h.n}")
      assert(values == List(1, 2), s"values=$values")
    }
    FutureCompleter(c)
  }

  // a local val, a path through vals, `this` (not bound) and an object (bound with its singleton type): only the final values are checked
  @Test def testStableQualifiers() = {
    val h = new Holder
    val nested = new Nested(new Holder)
    val c = async[Future] {
      h.n = await(Future.successful(1))
      nested.holder.n = await(Future.successful(2))
      HolderObject.n = await(Future.successful(3))
      await(nested.setThis(4))
      assert(h.n == 1, s"h.n=${h.n}")
      assert(nested.holder.n == 2, s"nested.holder.n=${nested.holder.n}")
      assert(HolderObject.n == 3, s"HolderObject.n=${HolderObject.n}")
      assert(nested.m == 4, s"nested.m=${nested.m}")
    }
    FutureCompleter(c)
  }

}

object TestAssignQualifierEvaluationOrder {

  class Holder {
    var n: Int = 0
    var fs: List[Future[Int]] = Nil
  }

  class Nested(val holder: Holder) {
    var m: Int = 0
    def setThis(x: Int): Future[Unit] = async[Future] {
      this.m = await(Future.successful(x))
    }
  }

  object HolderObject {
    var n: Int = 0
  }

  // only `testObjectQualifierInitialization` touches InitLoggedObject
  val objectLog = ArrayBuffer[String]()

  // only `testImportedMemberObjectInitialization` touches ImportedInitLoggedObject
  val importedObjectLog = ArrayBuffer[String]()

  object ImportedInitLoggedObject {
    importedObjectLog += "init"
    var n: Int = 0
  }

  object InitLoggedObject {
    objectLog += "init"
    var n: Int = 0
  }

  abstract class SelfBox {
    type T
    var v: T
    def mk: T
    def self(): this.type = this
    def assignSelf(): Future[Unit] = async[Future] {
      self().v = await(Future.successful(mk))
    }
  }

  def intSelfBox(x: Int): SelfBox = new SelfBox {
    type T = Int
    var v: Int = 0
    def mk: Int = x
  }

  class ByNameRef(h: => Holder, log: ArrayBuffer[String]) {
    def assign(x: Int): Future[Unit] = async[Future] {
      h.n = await({ log += "right"; Future.successful(x) })
    }
  }

  trait Box {
    type T
    var v: T
    def mk: T
  }

  def intBox(x: Int): Box = new Box {
    type T = Int
    var v: Int = 0
    def mk: Int = x
  }

}
