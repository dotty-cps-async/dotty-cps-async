package cpstest

import cps.*
import cps.monads.{*, given}

object CompileIssue98 {

  def notWork[F[_]: CpsAsyncMonad]: F[((String, Int), Option[String])] = async[F] {
    System.currentTimeMillis % 2 match
      case 0 =>
        ("k" -> await(1.pure), Option.empty)
      case 1 =>
        ("k" -> await(1.pure), Option("foo"))
  }

  extension [A](a: A) {
    def pure[F[_]: CpsAsyncMonad]: F[A] = summon[CpsAsyncMonad[F]].pure(a)
  }

  def callme(): Unit = {
    val r = notWork[AsyncFreeMonad]
    println(s"r=$r")
  }

}
