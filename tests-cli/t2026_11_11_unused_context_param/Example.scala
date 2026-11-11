//> using scala 3.9.0
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.4.1-SNAPSHOT
//> using options -Wunused:all -Werror
// see: https://github.com/dotty-cps-async/dotty-cps-async/issues/143
// Each async block used to warn "unused explicit parameter" when the transformed
// body does not use the monad context (the parameter of the rebuilt context lambda).

import cps.*
import cps.monads.{*, given}
import cps.stream.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

@main def test(): Unit =
  // body does not use the monad context
  val f1 = async[Future] { await(Future(1)) + 1 }
  // no awaits at all
  val f2 = async[Future] { 2 }
  // context is used: a nested async picks it up
  val f3 = async[Future] {
    val x = await(Future(3))
    await(async[Future] { x + 1 })
  }
  val stream = asyncStream[AsyncList[Future, Int]] { out =>
    out.emit(4)
  }
  val f4 = stream.fold(0)(_ + _)
  val r = Await.result(Future.sequence(List(f1, f2, f3, f4)), 5.seconds)
  println(r)
  assert(r == List(2, 2, 4, 4))
