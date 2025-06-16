package cpstest

import scala.concurrent.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}

import org.junit.*

// will be uncomment with scala-3.7

/*
object TestSiftSIP47Scope {

  def method[A](using String)[B](arg1: String, arg2: String): Future[String] = {
    Future.successful(s"${summon[String]}: ${arg1}  ${arg2}")
  }

}

class TestShiftSIP47 {

  import TestSiftSIP47Scope.*

  @Test
  def testShiftSIP47(): Unit = {
    given cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)
    async[Future] {
      given String = "Hello"
      val result = await(method("arg1", "arg2"))
    }

  }

}
 */
