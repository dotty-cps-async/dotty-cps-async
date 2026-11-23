package cps

import org.junit.Test

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

import cps.testconfig.given
import cps.monads.given
import cps.util.FutureCompleter

/** A selection of a val from a reference, which is null, throws before a later async part, as in Scala: the selection stays after
  * the async part (it is a pure path), the reference is checked in its place. See
  * https://github.com/dotty-cps-async/dotty-cps-async/issues/170 and https://github.com/dotty-cps-async/dotty-cps-async/issues/184.
  * JVM only: on Scala.js a null dereference is undefined behavior, not a `NullPointerException`.
  */
class TestNullReferenceSelectionOrder {

  import TestNullReferenceSelectionOrder.*

  // a sync argument
  @Test def testArg() = {
    val log = ArrayBuffer[String]()
    val t: Token = null
    val c = async[Future] {
      pair(t.x, await({ log += "second"; Future.successful(0) }))
    }
    FutureCompleter(expectNullPointerException(c, log, Nil))
  }

  // a path through a val, which is null: `a` is not null, `a.token` is
  @Test def testNestedPathArg() = {
    val log = ArrayBuffer[String]()
    val a = new TokenHolder(null)
    val c = async[Future] {
      pair(a.token.x, await({ log += "second"; Future.successful(0) }))
    }
    FutureCompleter(expectNullPointerException(c, log, Nil))
  }

  // a val imported from a reference, which is null
  @Test def testImportedArg() = {
    val log = ArrayBuffer[String]()
    val t: Token = null
    val c = async[Future] {
      import t.x
      pair(x, await({ log += "second"; Future.successful(0) }))
    }
    FutureCompleter(expectNullPointerException(c, log, Nil))
  }

  // a named argument, an element of varargs and an argument in the first argument list of a curried call
  @Test def testNamedVarargsCurriedArgs() = {
    val log = ArrayBuffer[String]()
    val t: Token = null
    // started one after another: they share the log
    def c1 = async[Future] {
      pair(a = t.x, b = await({ log += "second"; Future.successful(0) }))
    }
    def c2 = async[Future] {
      List(t.x, await({ log += "second"; Future.successful(0) }))
    }
    def c3 = async[Future] {
      curried(t.x)(await({ log += "second"; Future.successful(0) }))
    }
    FutureCompleter(for {
      _ <- expectNullPointerException(c1, log, Nil)
      _ <- expectNullPointerException(c2, log, Nil)
      _ <- expectNullPointerException(c3, log, Nil)
    } yield ())
  }

  // the async argument is before the selection: it is evaluated, then the selection throws
  @Test def testArgAfterAsync() = {
    val log = ArrayBuffer[String]()
    val t: Token = null
    val c = async[Future] {
      pair(await({ log += "second"; Future.successful(0) }), t.x)
    }
    FutureCompleter(expectNullPointerException(c, log, List("second")))
  }

  // a qualifier of a method call
  @Test def testQualifier() = {
    val log = ArrayBuffer[String]()
    val t: Token = null
    val c = async[Future] {
      t.holder.plus(await({ log += "second"; Future.successful(0) }))
    }
    FutureCompleter(expectNullPointerException(c, log, Nil))
  }

  // a qualifier of an assignment
  @Test def testAssignQualifier() = {
    val log = ArrayBuffer[String]()
    val t: Token = null
    val c = async[Future] {
      t.holder.n = await({ log += "second"; Future.successful(0) })
    }
    FutureCompleter(expectNullPointerException(c, log, Nil))
  }

  // the references are not null: the paths are evaluated after the async part
  @Test def testNotNull() = {
    val t = new Token(1, new Holder)
    val a = new TokenHolder(t)
    val c = async[Future] {
      val r1 = pair(a.token.x, await(Future.successful(2)))
      a.token.holder.n = await(Future.successful(3))
      val r2 = a.token.holder.plus(await(Future.successful(4)))
      assert(r1 == (1, 2), s"r1=$r1")
      assert(t.holder.n == 3, s"n=${t.holder.n}")
      assert(r2 == 5, s"r2=$r2")
    }
    FutureCompleter(c)
  }

}

object TestNullReferenceSelectionOrder {

  class Holder {
    var n: Int = 0
    def plus(y: Int): Int = y + 1
  }

  class Token(val x: Int, val holder: Holder)

  class TokenHolder(val token: Token)

  def pair(a: Int, b: Int): (Int, Int) = (a, b)

  def curried(a: Int)(b: Int): (Int, Int) = (a, b)

  def expectNullPointerException[A](c: Future[A], log: ArrayBuffer[String], expectedLog: List[String]): Future[Unit] =
    c.transform {
      case Failure(ex: NullPointerException) =>
        val r =
          if (log.toList == expectedLog) then Success(())
          else Failure(AssertionError(s"log=$log, expected $expectedLog"))
        log.clear()
        r
      case other => Failure(AssertionError(s"NullPointerException expected, got $other"))
    }

}
