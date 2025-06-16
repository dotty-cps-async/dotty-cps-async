package cpstest

import scala.concurrent.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}

import org.junit.*

object TestSiftSIP47Scope {

  def method[A](using String)[B](arg1: String, arg2: String): Future[String] = {
    Future.successful(s"${summon[String]}: ${arg1}  ${arg2}")
  }

  // kyo-like definitions
  trait F
  trait P[E,A]
  
  given F = new F {}
  given P[String,Int] = new P {}

  def method1[A](using F)(name: String, default: =>A)[E](using P[E,A]): Future[Either[E,A]] = ???

}

class TestShiftSIP47 {

  import TestSiftSIP47Scope.{*,given}

  @Test
  def testShiftSIP47(): Unit = {
    //given cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)
    async[Future] {
      given String = "Hello"
      val result = await(method("arg1", "arg2"))
    }
  }

  @Test
  def testShiftSIP47KioLike(): Unit = {
     val f = async[Future] {
        val e:Either[String, Int] = method1("toto",  2).await
        e
     }
  }

}
