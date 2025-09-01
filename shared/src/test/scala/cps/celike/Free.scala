package cps.celike

sealed abstract class Free[S[_], A] {

  def map[B](f: A => B): Free[S, B] =
    ???

  def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    ???

  def run: S[A]

}
