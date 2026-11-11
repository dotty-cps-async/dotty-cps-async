//> using scala 3.9.0
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.4.1-SNAPSHOT
//> using options -Wunused:all -Werror
// see: https://github.com/dotty-cps-async/dotty-cps-async/issues/148
// `val (a, b) = e` inside async was reported as "unused pattern variable": the compiler-generated `$1$`
// of the desugared match lost its derived name when the macro copied the `Bind`.

import cps.*
import cps.monads.{*, given}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

case class P(x: Int, y: Int)

def pair(): (Int, Int) = (1, 2)

@main def test(): Unit =
  // awaited value destructured
  val f1 = async[Future] {
    val (a, b) = await(Future(pair()))
    a + b
  }
  // no awaits
  val f2 = async[Future] {
    val (a, b) = pair()
    a + b
  }
  // destructuring after await
  val f3 = async[Future] {
    val x = await(Future(1))
    val (a, b) = pair()
    a + b + x
  }
  // case class
  val f4 = async[Future] {
    val P(a, b) = await(Future(P(1, 2)))
    a * b
  }
  // plain match with used binds
  val f5 = async[Future] {
    await(Future(pair())) match
      case (a, b) => a - b
  }
  val r = Await.result(Future.sequence(List(f1, f2, f3, f4, f5)), 5.seconds)
  println(r)
  assert(r == List(3, 3, 4, 2, -1))
