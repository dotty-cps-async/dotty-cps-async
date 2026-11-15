package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

// a call of a @makeCPS method, deferred by the macro to the plugin: the qualifier and the async argument are evaluated once,
//  in this order (#169)
class Test12m5Holder {
  @cps.plugin.annotation.makeCPS
  def m(a: Int, f: Int => Int): Int = f(a)
}

object Test12m5 {

  var log = List.empty[String]

  val holder = new Test12m5Holder

  def track(): Test12m5Holder = { log = log :+ "qualifier"; holder }

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      track().m(await({ log = log :+ "arg"; Future.successful(1) }), x => x + await(Future.successful(2)))
    }
    val r = Await.result(fr, 1000.millis)
    println(s"$r ${log.mkString(",")}")
  }
}
