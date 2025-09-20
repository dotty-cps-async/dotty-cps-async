package cpstest

import cps.*

object CompileIssue112:

  implicit inline def debugLevel: cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(53)

  extension (s: String)
    def baz[F[_]: CpsMonad]: F[F[Int]] = async[F]:
      // s.baa(foo.await).await
      s.baa(foo.await)

    def baa[F[_]: CpsMonad](param: String): F[Int] = ???
  end extension

  def foo[F[_]: CpsMonad]: F[String] = ???
