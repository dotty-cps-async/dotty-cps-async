package cps.monads

import cps.*

import scala.reflect.ClassTag
import scala.util.{Failure, NotGiven, Success}

trait ThrowableMapping[E] {

  def toThrowable(e: E): Option[Throwable]

  def fromThrowable(t: Throwable): Option[E]

}

object ThrowableMapping {

  given [E <: Throwable: ClassTag]: ThrowableMapping[E] with {
    def toThrowable(e: E): Option[Throwable] = Some(e)
    def fromThrowable(t: Throwable): Option[E] =
      summon[ClassTag[E]].unapply(t).map(_.asInstanceOf[E])
  }

}

class EitherCpsTryMonad[E: ThrowableMapping]
    extends CpsTryMonad[[A] =>> Either[E, A]]
    with CpsTryMonadInstanceContext[[A] =>> Either[E, A]] {

  val tm = summon[ThrowableMapping[E]]

  override def pure[A](a: A): Either[E, A] = Right(a)

  override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa match {
    case Left(e) => Left(e)
    case Right(a) =>
      try Right(f(a))
      catch
        case ex: Throwable =>
          tm.fromThrowable(ex) match {
            case Some(ee) => Left(ee)
            case None     => throw ex
          }
  }

  override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
    case Left(e) => Left(e)
    case Right(a) =>
      handleRun(f(a))
  }

  override def error[A](e: Throwable): Either[E, A] = tm.fromThrowable(e) match {
    case Some(ee) => Left(ee)
    case None     => throw e
  }

  override def flatMapTry[A, B](fa: Either[E, A])(f: scala.util.Try[A] => Either[E, B]): Either[E, B] = fa match {
    case Left(e) =>
      tm.toThrowable(e) match {
        case Some(te) =>
          handleRun(f(Failure(te)))
        case None => Left(e)
      }
    case Right(a) =>
      handleRun(f(scala.util.Success(a)))
  }

  private def handleRun[A](op: => Either[E, A]): Either[E, A] = {
    try op
    catch
      case ex: Throwable =>
        tm.fromThrowable(ex) match {
          case Some(ee) => Left(ee)
          case None     => throw ex
        }
  }

}

given eitherTryCpsMonad[E: ThrowableMapping]: CpsTryMonad[[A] =>> Either[E, A]] =
  new EitherCpsTryMonad[E]

given eitherPureCpsMonad[E](using NotGiven[ThrowableMapping[E]]): CpsThrowMonad[[A] =>> Either[E, A]] =
  new EitherCpsMonad[E]

class EitherCpsMonad[E](using ev: NotGiven[ThrowableMapping[E]])
    extends CpsThrowMonad[[A] =>> Either[E, A]]
    with CpsThrowMonadInstanceContext[[A] =>> Either[E, A]] {

  override def pure[A](a: A): Either[E, A] = Right(a)

  override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa match {
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))
  }

  override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }

  override def error[A](e: Throwable): Either[E, A] = throw e

}
