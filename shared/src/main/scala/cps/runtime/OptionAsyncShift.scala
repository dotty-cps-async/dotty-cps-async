package cps.runtime

import scala.util._
import cps._

class OptionAsyncShift[T] extends AsyncShift[Option[T]]:

  def exists[F[_]](o: Option[T], m: CpsMonad[F])(p: T => F[Boolean]): F[Boolean] =
    o match
      case Some(t) => p(t)
      case None    => m.pure(false)

  def collect[F[_], U](o: Option[T], m: CpsMonad[F])(pf: PartialFunction[T, F[U]]): F[Option[U]] =
    o match
      case Some(t) =>
        pf.lift(t) match
          case Some(fu) => m.map(fu)(Some(_))
          case None     => m.pure(None)
      case None => m.pure(None)

  def filter[F[_]](o: Option[T], m: CpsMonad[F])(p: T => F[Boolean]): F[Option[T]] =
    o match
      case Some(t) =>
        m.map(p(t)) { r =>
          o.filter(_ => r)
        }
      case None => m.pure(None)

  def filterNot[F[_]](o: Option[T], m: CpsMonad[F])(p: T => F[Boolean]): F[Option[T]] =
    o match
      case Some(t) =>
        m.map(p(t)) { r =>
          o.filterNot(_ => r)
        }
      case None => m.pure(None)

  def find[F[_]](o: Option[T], m: CpsMonad[F])(p: T => F[Boolean]): F[Option[T]] =
    o match
      case Some(t) => m.map(p(t))(r => if r then Some(t) else None)
      case None    => m.pure(None)

  def fold[F[_], U](o: Option[T], m: CpsMonad[F])(ifEmpty: () => F[U])(f: T => F[U]): F[U] =
    o match
      case Some(t) => f(t)
      case None    => ifEmpty()

  def flatMap[F[_], U](o: Option[T], m: CpsMonad[F])(f: (T) => F[Option[U]]): F[Option[U]] =
    o match
      case Some(t) => f(t)
      case None    => m.pure(None)

  def forall[F[_]](o: Option[T], m: CpsMonad[F])(p: T => F[Boolean]): F[Boolean] =
    o match
      case Some(t) => p(t)
      case None    => m.pure(true)

  def foreach[F[_], U](o: Option[T], m: CpsMonad[F])(f: (T) => F[U]): F[Unit] =
    o match
      case Some(t) => m.map(f(t))(_ => ())
      case None    => m.pure(())

  def getOrElse[F[_], U >: T](o: Option[T], m: CpsMonad[F])(default: () => F[U]): F[U] =
    o match
      case Some(t) => m.pure(t)
      case None    => default()

  def map[F[_], U](o: Option[T], m: CpsMonad[F])(f: (T) => F[U]): F[Option[U]] =
    o match
      case Some(t) => m.map(f(t))(x => Some(x))
      case None    => m.pure(None)

  def orElse[F[_], U >: T](o: Option[T], m: CpsMonad[F])(default: () => F[Option[U]]): F[Option[U]] =
    o match
      case Some(t) => m.pure(Some(t))
      case None    => default()
