package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}
import cps.util.FutureCompleter

import org.junit.Test

opaque type Issue99Maybe1[A] = A | Issue99Maybe1.Absent.type

object Issue99Maybe1 {

  object Absent

  def apply[A](a: A): Issue99Maybe1[A] = a.asInstanceOf[Issue99Maybe1[A]]

  def empty[A]: Issue99Maybe1[A] = Absent

  def isEmpty[A](m: Issue99Maybe1[A]): Boolean = m.isInstanceOf[Absent.type]

  extension [A](self: Issue99Maybe1[A]) {

    def map[B](f: A => B): Issue99Maybe1[B] = self match {
      case Absent => Absent
      case a: A   => f(a).asInstanceOf[Issue99Maybe1[B]]
    }

    def map_async[F[_]: CpsAsyncMonad, B](f: A => F[B]): F[Issue99Maybe1[B]] = self match {
      case Absent => summon[CpsAsyncMonad[F]].pure(Absent)
      case a: A   => summon[CpsAsyncMonad[F]].map(f(a))(b => b.asInstanceOf[Issue99Maybe1[B]])
    }

  }

}

opaque type Issue99Maybe2[A] = A | Issue99Maybe2.Absent.type

object Issue99Maybe2 {

  object Absent

  def apply[A](a: A): Issue99Maybe2[A] = a.asInstanceOf[Issue99Maybe2[A]]

  def empty[A]: Issue99Maybe2[A] = Absent

  def isEmpty[A](m: Issue99Maybe2[A]): Boolean = m.isInstanceOf[Absent.type]

  extension [A](self: Issue99Maybe2[A]) {

    def map[B](f: A => B): Issue99Maybe2[B] = self match {
      case Absent => Absent
      case a: A   => f(a).asInstanceOf[Issue99Maybe2[B]]
    }

  }

  class Maybe2AsyncShift[T] extends cps.AsyncShift[Issue99Maybe2[T]] {

    def map[F[_], A, B](obj: Issue99Maybe2[A], cpsMonad: CpsMonad[F])(f: A => F[B]): F[Issue99Maybe2[B]] = obj match {
      case Absent => cpsMonad.pure(Absent)
      case a: A   => cpsMonad.map(f(a))(b => b.asInstanceOf[Issue99Maybe2[B]])
    }

  }

  given maybeAsyncShift[T]: AsyncShift[Issue99Maybe2[T]] = new Maybe2AsyncShift[T]

}

class TestOpaqueAsyncShift {

  /*
  @Test
  def testShiftedMapOnOpaqueType1() = {
    val f = async[Future] {
      val m = Issue99Maybe2(42)
      val r = m.map(x => Future.successful(x + 1).await)
      assert(r == Issue99Maybe2(43))
      r
    }
  }

   */

  /*
  @Test
  def testShiftedMapOnOpaqueType2() = {
    val f = async[Future] {
      val m = Issue99Maybe2(42)
      val r = m.map(x => Future.successful(x + 1).await)
      assert(r == Issue99Maybe2(43))
      r
    }
  }

   */

}
