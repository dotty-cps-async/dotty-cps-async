package cpstest.s4.m4

import scala.annotation.experimental
import cps._
import cps.monads.{*,given}
import testUtil.*


@experimental
object Test4m4 {

  def fetch[A](a: A): FreeMonad[A] = FreeMonad.Pure(a)

  // transformed by the compiler plugin
  def directRuntimeChecked(o: Option[Int])(using CpsDirect[FreeMonad]): Int =
    await(fetch(o)).runtimeChecked match
      case Some(x) => x + 1

  def directUnchecked(o: Option[Int])(using CpsDirect[FreeMonad]): Int =
    (await(fetch(o)): @unchecked) match
      case Some(x) => x + await(fetch(1))

  def directValPattern(o: Option[Int])(using CpsDirect[FreeMonad]): Int =
    val Some(x) = await(fetch(o)).runtimeChecked
    x + 1

  // runtimeChecked is inline: async argument goes to the proxy binding of Inlined, as here
  inline def plusOne(x: Int): Int = x + 1

  def directInline(o: Int)(using CpsDirect[FreeMonad]): Int =
    plusOne(await(fetch(o)))

  // transformed by the async macro
  def asyncRuntimeChecked(o: Option[Int]): FreeMonad[Int] = async[FreeMonad] {
    await(fetch(o)).runtimeChecked match
      case Some(x) => x + await(fetch(1))
  }

  def asyncUnchecked(o: Option[Int]): FreeMonad[Int] = async[FreeMonad] {
    (await(fetch(o)): @unchecked) match
      case Some(x) => x + 1
  }

  def main(args:Array[String]):Unit = {
    val r1 = reify[FreeMonad] { directRuntimeChecked(Some(1)) }.eval
    val r2 = reify[FreeMonad] { directUnchecked(Some(1)) }.eval
    val r3 = reify[FreeMonad] { directValPattern(Some(2)) }.eval
    val r4 = asyncRuntimeChecked(Some(3)).eval
    val r5 = asyncUnchecked(Some(4)).eval
    val r6 = reify[FreeMonad] { directRuntimeChecked(None) }.tryEval
    val r7 = asyncRuntimeChecked(None).tryEval
    val r8 = reify[FreeMonad] { directInline(5) }.eval
    println(s"$r1 $r2 $r3 $r4 $r5 ${r6.isFailure} ${r7.isFailure} $r8")
  }

}
