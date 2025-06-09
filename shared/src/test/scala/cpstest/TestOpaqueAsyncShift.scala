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

    def map_async[F[_], B](m: CpsAsyncMonad[F])(f: A => F[B]): F[Issue99Maybe1[B]] = self match {
      case Absent => m.pure(Absent)
      case a: A   => m.map(f(a))(b => b.asInstanceOf[Issue99Maybe1[B]])
    }

  }

  // def map_async[F[_], A](cpsMonad: CpsAsyncMonad[F])(m: Issue99Maybe1[A])[B](f: A => F[B]): F[Issue99Maybe1[B]] = m match {
  //  case Absent => cpsMonad.pure(Absent)
  //  case a: A   => cpsMonad.map(f(a))(b => b.asInstanceOf[Issue99Maybe1[B]])
  // }

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

opaque type Issue99MyStringList = List[String]

object Issue99MyStringList {

  def apply(xs: String*): Issue99MyStringList = xs.toList

  extension (self: Issue99MyStringList) {

    def isEmpty: Boolean = self.isEmpty

    def head: String = self.head

    def map(f: String => String): Issue99MyStringList = self.map(f)

    def map_async[F[_]](m: CpsAsyncMonad[F])(f: String => F[String]): F[Issue99MyStringList] = {
      self
        .foldRight(m.pure(List.empty[String])) { (e, acc) =>
          m.flatMap(acc) { list =>
            m.map(f(e))(b => b :: list)
          }
        }
        .asInstanceOf[F[Issue99MyStringList]]
    }

  }

}

class TestOpaqueAsyncShift {

  @Test
  def testShiftedMapOnOpaqueType1() = {
    // implicit val debugLevel: cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)
    val f = async[Future] {
      val m = Issue99Maybe1(42)
      val r = m.map(x => Future.successful(x + 1).await)
      assert(r == Issue99Maybe1(43))
      r
    }
    FutureCompleter(f)
  }

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

  @Test
  def testOpaqueShiftWithoutTypeParams() = {
    // implicit val debugLevel: cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)
    val f = async[Future] {
      val m = Issue99MyStringList("a", "b", "c")
      val r = m.map(x => Future.successful(x + "a").await)
      assert(r.head == "aa")
      r
    }
    FutureCompleter(f)
  }

}
