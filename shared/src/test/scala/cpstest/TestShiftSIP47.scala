package cpstest

import scala.concurrent.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}
import cps.util.FutureCompleter

import org.junit.*
import org.junit.Assert.*

object TestSiftSIP47Scope {

  def method[A](using String)[B](arg1: String, arg2: String): Future[String] = {
    Future.successful(s"${summon[String]}: ${arg1}  ${arg2}")
  }

  def pure[A](a: A)[B](b: B): (A, B) = (a, b)

}

class TestShiftSIP47 {

  import TestSiftSIP47Scope.*

  @Test
  def testShiftSIP47(): Unit = {
    val f = async[Future] {
      given String = "Hello"
      val result = await(method("arg1", "arg2"))
      assertEquals("Hello: arg1  arg2", result)
    }
    FutureCompleter(f)
  }

  @Test
  def testSIP47AwaitInArgs(): Unit = {
    val f = async[Future] {
      given String = "Hello"
      val result = await(method[Int](using summon[String])[Long](await(Future.successful("a1")), await(Future.successful("a2"))))
      assertEquals("Hello: a1  a2", result)
    }
    FutureCompleter(f)
  }

  @Test
  def testSIP47AwaitInBothClauses(): Unit = {
    val f = async[Future] {
      val r = pure[Int](await(Future.successful(1)))[String](await(Future.successful("x")))
      assertEquals((1, "x"), r)
    }
    FutureCompleter(f)
  }

}
