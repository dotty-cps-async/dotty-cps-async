//> using scala 3.9.0
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.4.1-SNAPSHOT
// see: https://github.com/dotty-cps-async/dotty-cps-async/issues/153
// The cases of an async partial function are copied into `isDefinedAt` and `apply` with new bind symbols.
// Macro mode lost the binds collected before a nested extractor (`a` in `(a, (b, c))`, `p` in `p @ (a, b)`),
// and did not rebind binds under a type test (`b` in `(a, Some(b))`) or a named argument.

import cps.*
import cps.monads.{*, given}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

case class P(x: Int, y: Int)

@main def test(): Unit =
  // bind before a nested extractor
  val f1 = async[Future] {
    List((1, (2, 3))).collect { case (a, (b, c)) => await(Future(a + b + c)) }.sum
  }
  // bind over an extractor
  val f2 = async[Future] {
    List((1, 2)).collect { case p @ (a, b) => await(Future(p._1 + a + b)) }.sum
  }
  // extractor under a type test (element type is Option[Int])
  val f3 = async[Future] {
    List((1, Some(2)), (3, None)).collect { case (a, Some(b)) => await(Future(a + b)) }.sum
  }
  // named pattern
  val f4 = async[Future] {
    List(P(1, 2)).collect { case P(y = b) => await(Future(b)) }.sum
  }
  // guard with binds of a nested extractor
  val f5 = async[Future] {
    List((1, (2, 3)), (4, (5, 6))).collect { case (a, (b, c)) if a > b - 2 => await(Future(a + b + c)) }.sum
  }
  val r = Await.result(Future.sequence(List(f1, f2, f3, f4, f5)), 5.seconds)
  println(r)
  assert(r == List(6, 4, 3, 2, 21))
