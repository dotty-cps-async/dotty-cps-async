package cps.monads

import cps.{CpsTryMonad, CpsTryMonadInstanceContext}

import scala.util.{Success, Try}

/** Dependency-less version of IdentityMonad
  * @tparam T
  */
type CpsIdentity[T] = T

object CpsIdentityMonad extends CpsTryMonad[CpsIdentity] with CpsTryMonadInstanceContext[CpsIdentity] {

  override def pure[A](a: A): A = a

  override def map[A, B](fa: A)(f: A => B): B = f(fa)

  override def flatMap[A, B](fa: A)(f: A => B): B = f(fa)

  override def error[A](e: Throwable): A = throw e

  override def flatMapTry[A, B](fa: A)(f: Try[A] => B): B = f(Success(fa))

  /** Stack-safe implementation using while loop */
  override def tailRecM[A, B](a: A)(f: A => Either[A, B]): B = {
    var current: Either[A, B] = Left(a)
    while (current.isLeft) {
      current = f(current.left.get)
    }
    current.right.get
  }

  override def toString = "CpsIdentityMonad"

}

given CpsTryMonad[CpsIdentity] = CpsIdentityMonad
