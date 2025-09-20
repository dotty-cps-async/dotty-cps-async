//> using scala 3.7.1
//> using dep org.typelevel::cats-effect:3.5.4
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.1.5-SNAPSHOT
//> using dep io.github.dotty-cps-async::cps-async-connect-cats-effect:1.1.4

import cats.effect.*
import cps.*
import cps.monads.catsEffect.{*, given}

object Minimization:
  extension (s: String)
    def baz[F[_]: Async]: F[Int] = async[F]:
      s.baa(foo.await).await

    def baa[F[_]: Async](param: String): F[Int] = ???
  end extension

  def foo[F[_]: Async]: F[String] = ???