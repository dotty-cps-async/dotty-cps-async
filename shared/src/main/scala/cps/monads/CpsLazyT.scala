package cps.monads

import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal

import cps.{CpsTryMonad, CpsTryMonadInstanceContext, CpsTryEffectMonad}


/**
 * Lazy monad transformer providing stack-safe trampolined evaluation
 * for any CpsTryMonad[F].
 *
 * The ADT nodes capture computations as data structures rather than
 * evaluating them immediately, allowing the `run` interpreter to
 * process them with an explicit continuation stack via `tailRecM`.
 */
sealed trait CpsLazyT[F[_], +A]

object CpsLazyT {

  case class Pure[F[_], A](value: A) extends CpsLazyT[F, A]
  case class Error[F[_], A](e: Throwable) extends CpsLazyT[F, A]
  case class Lift[F[_], A](fa: F[A]) extends CpsLazyT[F, A]
  case class Delay[F[_], A](thunk: () => CpsLazyT[F, A]) extends CpsLazyT[F, A]
  case class FlatMap[F[_], A, B](la: CpsLazyT[F, A], f: A => CpsLazyT[F, B]) extends CpsLazyT[F, B]
  case class FlatMapTry[F[_], A, B](la: CpsLazyT[F, A], f: Try[A] => CpsLazyT[F, B]) extends CpsLazyT[F, B]

  def pure[F[_], A](a: A): CpsLazyT[F, A] = Pure(a)

  def lift[F[_], A](fa: F[A]): CpsLazyT[F, A] = Lift(fa)

  def delay[F[_], A](thunk: => A): CpsLazyT[F, A] = Delay(() => Pure(thunk))

  def flatDelay[F[_], A](thunk: => CpsLazyT[F, A]): CpsLazyT[F, A] = Delay(() => thunk)

  def error[F[_], A](e: Throwable): CpsLazyT[F, A] = Error(e)

  // Stack element: Left = flatMap continuation, Right = flatMapTry continuation
  // We erase the value type to Any but keep F[_] correct.
  private type Stack[F[_]] = List[Either[Any => CpsLazyT[F, Any], Try[Any] => CpsLazyT[F, Any]]]
  private type State[F[_]] = (CpsLazyT[F, Any], Stack[F])

  /**
   * Stack-safe interpreter that evaluates a CpsLazyT tree into the
   * underlying monad F. Uses FM.tailRecM with an explicit continuation
   * stack to avoid JVM stack overflow.
   */
  def run[F[_], A](lt: CpsLazyT[F, A])(using FM: CpsTryMonad[F]): F[A] = {

    def applyStack(value: Any, stack: Stack[F]): Either[State[F], Any] = {
      stack match {
        case Nil => Right(value)
        case Left(f) :: rest =>
          try {
            Left((f(value), rest))
          } catch {
            case NonFatal(ex) =>
              applyErrorStack(ex, rest)
          }
        case Right(f) :: rest =>
          try {
            Left((f(Success(value)), rest))
          } catch {
            case NonFatal(ex) =>
              applyErrorStack(ex, rest)
          }
      }
    }

    def applyErrorStack(e: Throwable, stack: Stack[F]): Either[State[F], Any] = {
      stack match {
        case Nil => throw e
        case Left(_) :: rest =>
          // Skip flatMap continuations when in error state
          applyErrorStack(e, rest)
        case Right(f) :: rest =>
          try {
            Left((f(Failure(e)), rest))
          } catch {
            case NonFatal(ex) =>
              applyErrorStack(ex, rest)
          }
      }
    }

    def step(state: State[F]): F[Either[State[F], Any]] = {
      val (current, stack) = state
      current match {
        case Pure(v) =>
          FM.pure(applyStack(v, stack))

        case Error(e) =>
          applyErrorStack(e, stack) match {
            case r @ Right(_) => FM.pure(r)
            case l @ Left(_)  => FM.pure(l)
          }

        case Delay(thunk) =>
          try {
            FM.pure(Left((thunk().asInstanceOf[CpsLazyT[F, Any]], stack)))
          } catch {
            case NonFatal(ex) =>
              applyErrorStack(ex, stack) match {
                case r @ Right(_) => FM.pure(r)
                case l @ Left(_)  => FM.pure(l)
              }
          }

        case fm: FlatMap[F @unchecked, a, b] =>
          val castF = fm.f.asInstanceOf[Any => CpsLazyT[F, Any]]
          FM.pure(Left((fm.la.asInstanceOf[CpsLazyT[F, Any]], Left(castF) :: stack)))

        case fmt: FlatMapTry[F @unchecked, a, b] =>
          val castF = fmt.f.asInstanceOf[Try[Any] => CpsLazyT[F, Any]]
          FM.pure(Left((fmt.la.asInstanceOf[CpsLazyT[F, Any]], Right(castF) :: stack)))

        case Lift(fa) =>
          FM.flatMapTry(fa.asInstanceOf[F[Any]]) { tryResult =>
            tryResult match {
              case Success(v) =>
                applyStack(v, stack) match {
                  case r @ Right(_) => FM.pure(r)
                  case l @ Left(_)  => FM.pure(l)
                }
              case Failure(e) =>
                applyErrorStack(e, stack) match {
                  case r @ Right(_) => FM.pure(r)
                  case l @ Left(_)  => FM.pure(l)
                }
            }
          }
      }
    }

    FM.tailRecM[State[F], Any]((lt.asInstanceOf[CpsLazyT[F, Any]], Nil))(step).asInstanceOf[F[A]]
  }

  /**
   * CpsTryEffectMonad instance for CpsLazyT[F, _].
   * Requires an underlying CpsTryMonad[F] in implicit scope.
   */
  given cpsLazyTMonad[F[_]](using FM: CpsTryMonad[F]): CpsTryEffectMonad[[X] =>> CpsLazyT[F, X]] with CpsTryMonadInstanceContext[[X] =>> CpsLazyT[F, X]] with {

    override def pure[A](a: A): CpsLazyT[F, A] = Pure(a)

    override def map[A, B](fa: CpsLazyT[F, A])(f: A => B): CpsLazyT[F, B] =
      FlatMap(fa, (a: A) => Pure(f(a)))

    override def flatMap[A, B](fa: CpsLazyT[F, A])(f: A => CpsLazyT[F, B]): CpsLazyT[F, B] =
      FlatMap(fa, f)

    override def error[A](e: Throwable): CpsLazyT[F, A] = Error(e)

    override def flatMapTry[A, B](fa: CpsLazyT[F, A])(f: Try[A] => CpsLazyT[F, B]): CpsLazyT[F, B] =
      FlatMapTry(fa, f)

    override def delay[A](x: => A): CpsLazyT[F, A] =
      Delay(() => Pure(x))

    override def flatDelay[A](x: => CpsLazyT[F, A]): CpsLazyT[F, A] =
      Delay(() => x)

    override def tailRecM[A, B](a: A)(f: A => CpsLazyT[F, Either[A, B]]): CpsLazyT[F, B] = {
      FlatMap(f(a), (either: Either[A, B]) => either match {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => Pure(b)
      })
    }

  }

}


/**
 * CpsLazy is CpsLazyT specialized to CpsIdentity -- a pure trampolined monad.
 */
type CpsLazy[A] = CpsLazyT[CpsIdentity, A]

object CpsLazy {

  def pure[A](a: A): CpsLazy[A] = CpsLazyT.pure[CpsIdentity, A](a)

  def delay[A](thunk: => A): CpsLazy[A] = CpsLazyT.delay[CpsIdentity, A](thunk)

  def flatDelay[A](thunk: => CpsLazy[A]): CpsLazy[A] = CpsLazyT.flatDelay[CpsIdentity, A](thunk)

  def error[A](e: Throwable): CpsLazy[A] = CpsLazyT.error[CpsIdentity, A](e)

  def run[A](la: CpsLazy[A]): A = CpsLazyT.run[CpsIdentity, A](la)(using CpsIdentityMonad)

}
