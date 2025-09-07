package cpstest

import cps.*

import scala.util.Try

trait LCHCursor {}

object LCHCursor {

  def fromLCJson(json: LCJson): LCHCursor =
    ???

}

sealed class LCJson {

  def hcursor: LCHCursor = LCHCursor.fromLCJson(this)

}

trait LCAsync[F[_]] {
  def async_[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
}

class LCAsyncCpsMonad[F[_]: LCAsync] extends CpsTryMonad[F] with CpsTryMonadInstanceContext[F] {

  override def pure[T](t: T): F[T] = ???

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = ???

  override def error[A](e: Throwable): F[A] = ???

  override def flatMapTry[A, B](fa: F[A])(f: Try[A] => F[B]): F[B] = ???

}

given [F[_]: LCAsync]: CpsTryMonad[F] = LCAsyncCpsMonad[F]()

object Example:

  // implicit val printCode: cps.macros.flags.PrintCode.type = cps.macros.flags.PrintCode
  // implicit inline def debugLevel: cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)

  def convertEitherStringToF[F[_]: LCAsync] = new CpsMonadConversion[[A] =>> Either[String, A], F]:
    def apply[A](e: Either[String, A]) =
      ???
      // e.leftMap(err => new RuntimeException(err)).liftTo[F]
  given eitherStringToFAsyncGiven[F[_]: LCAsync]: CpsMonadConversion[[A] =>> Either[String, A], F] = convertEitherStringToF

  def lookupBreak[F[_]: LCAsync](e: Either[String, LCJson]): F[LCHCursor] = async[F]:
    e.await.hcursor
    // e.get.hcursor
