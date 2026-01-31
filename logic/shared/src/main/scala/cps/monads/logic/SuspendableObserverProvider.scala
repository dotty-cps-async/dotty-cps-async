package cps.monads.logic

import cps.*
import cps.monads.CpsLazyT

import scala.util.NotGiven

trait SuspendableObserverProvider[F[_]] {
  type SO[X]
  def monad: CpsTryEffectMonad[SO]
  def runSuspended[A](sa: SO[A]): F[A]
  def suspend[A](oa: => F[A]): SO[A]
}

object SuspendableObserverProvider {

  type Aux[F[_], S[_]] = SuspendableObserverProvider[F] { type SO[X] = S[X] }

  def usingLazyT[F[_]](using fm: CpsTryMonad[F]): SuspendableObserverProvider[F] =
    new SuspendableObserverProvider[F] {
      type SO[X] = CpsLazyT[F, X]
      val monad: CpsTryEffectMonad[SO] = CpsLazyT.cpsLazyTMonad[F]
      def runSuspended[A](sa: CpsLazyT[F, A]): F[A] = CpsLazyT.run(sa)
      def suspend[A](oa: => F[A]): CpsLazyT[F, A] = CpsLazyT.Lift(oa)
    }

  def usingEffect[F[_]](using fm: CpsTryEffectMonad[F]): SuspendableObserverProvider[F] =
    new SuspendableObserverProvider[F] {
      type SO[X] = F[X]
      val monad: CpsTryEffectMonad[F] = fm
      def runSuspended[A](sa: F[A]): F[A] = sa
      def suspend[A](oa: => F[A]): F[A] = fm.flatDelay(oa)
    }

  given forEffectMonad[F[_]](using fm: CpsTryEffectMonad[F]): SuspendableObserverProvider[F] =
    usingEffect[F]

  given forTryMonad[F[_]](using fm: CpsTryMonad[F], ev: NotGiven[CpsTryEffectMonad[F]]): SuspendableObserverProvider[F] =
    usingLazyT[F]

}
