package cps.monads.logic
import scala.annotation.tailrec
import scala.collection.LazyZip2
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

trait LazyListCpsLogicMonadBase extends CpsSyncLogicMonad[LazyList] {

  def pure[T](t: T): LazyList[T] =
    LazyList(t)

  def map[A, B](fa: LazyList[A])(f: A => B): LazyList[B] =
    fa.map(f)

  def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
    fa.flatMap(f)

  override def error[A](e: Throwable): LazyList[A] = {
    LazyList.cons(throw e, LazyList.empty[A])
  }

  override def mapTry[A, B](fa: LazyList[A])(f: Try[A] => B): LazyList[B] = {
    if (fa.isEmpty) {
      LazyList.empty
    } else {
      val tryHead = Try(fa.head)
      LazyList.cons(f(tryHead), mapTry(fa.tail)(f))
    }
  }

  override def flatMapTry[A, B](fa: LazyList[A])(f: Try[A] => LazyList[B]): LazyList[B] = {
    if (fa.isEmpty) {
      LazyList.empty
    } else {
      val tryHead = Try(fa.head)
      val tryPrefix = Try(f(tryHead))
      tryPrefix match
        case Success(prefix) =>
          prefix #::: flatMapTry(fa.tail)(f)
        case Failure(ex) =>
          LazyList.cons(throw ex, flatMapTry(fa.tail)(f))
    }
  }

  override def mzero[A]: LazyList[A] = LazyList.empty

  override def mplus[A](x: LazyList[A], y: => LazyList[A]): LazyList[A] =
    if (x.isEmpty)
      y // actually, better be lazy here, but appropriate method in LazyList is private
    else
      LazyList.cons(x.head, mplus(x.tail, y))

  override def fsplit[A](c: LazyList[A]): Option[(Try[A], LazyList[A])] = {
    if (c.isEmpty)
      None
    else
      Some((Try(c.head), c.tail))
  }

  override def msplit[A](c: LazyList[A]): LazyList[Option[(Try[A], LazyList[A])]] = {
    if (c.isEmpty) {
      LazyList(None)
    } else {
      val tryHead = Try(c.head)
      val tail = c.tail
      LazyList(Some((tryHead, tail)))
    }
  }

  override def mObserveOne[A](ma: LazyList[A]): Option[A] =
    ma.headOption

  override def mFoldLeftWhileObserveM[A, B](ma: LazyList[A], zero: B, p: B => Boolean)(op: (B, A) => B): B = {

    @tailrec
    def go(ma: LazyList[A], zero: B): B = {
      if (ma.isEmpty) {
        zero
      } else {
        val head = ma.head
        val tail = ma.tail
        val newZero = op(zero, head)
        if (p(newZero)) {
          go(tail, newZero)
        } else {
          newZero
        }
      }
    }

    go(ma, zero)

  }

  override def mFoldM[S, A](ma: LazyList[A], s0: S)(op: (S, A) => LazyList[S]): LazyList[S] = {
    if (ma.isEmpty) {
      LazyList(s0)
    } else {
      val head = ma.head
      val tail = ma.tail
      val newS = op(s0, head)
      summon[CpsLogicMonad[LazyList]].flatMap(newS)(s => mFoldM(tail, s)(op))
    }
  }

  override final def toLazyList[T](m: LazyList[T]): LazyList[T] = m

}

object LazyListCpsLogicMonad extends LazyListCpsLogicMonadBase with CpsLogicMonadInstanceContext[LazyList]

given lazyListLogicMonad: CpsSyncLogicMonad[LazyList] = LazyListCpsLogicMonad
