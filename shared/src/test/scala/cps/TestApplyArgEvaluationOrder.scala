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

  // vals, a literal and `this` stay in the call
  @Test def testPurePathArgs() = {
    val c = async[Future] {
      val h = new Holder
      val y = 3
      val r = h.triple(y, 4, await(Future.successful(5)))
      assert(r == (3, 4, 5), s"r=$r")
      val s = await(h.withThis())
      assert(s == (h, 6), s"s=$s")
    }
    FutureCompleter(c)
  }

  // the call with a sync argument and a shifted lambda (no async argument): the argument is evaluated once
  @Test def testMethodCallArgWithShiftedLambda() = {
    val log = ArrayBuffer[String]()
    def first(): Int = { log += "first"; 1 }
    val c = async[Future] {
      val r = List(2, 3).foldLeft(first())((s, e) => s + await(Future.successful(e)))
      assert(log.toList == List("first"), s"log=$log")
      assert(r == 6, s"r=$r")
    }
    FutureCompleter(c)
  }

}

object TestApplyArgEvaluationOrder {

  def pair(a: Int, b: Int): (Int, Int) = (a, b)

  def curried(a: Int)(b: Int): (Int, Int) = (a, b)

  class Holder {
    var n: Int = 1
    def triple(a: Int, b: Int, c: Int): (Int, Int, Int) = (a, b, c)
    def withThis(): Future[(Holder, Int)] = async[Future] {
      pairThis(this, await(Future.successful(6)))
    }
    private def pairThis(h: Holder, b: Int): (Holder, Int) = (h, b)
  }

}
