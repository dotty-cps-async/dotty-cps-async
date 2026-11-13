//> using scala 3.9.0
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.4.1-SNAPSHOT
//> using options -Xcheck-macros -Wunused:all -Werror
// see: https://github.com/dotty-cps-async/dotty-cps-async/issues/155
// Macro mode built an async partial function with separate `isDefinedAt` and `apply`, so `collect` evaluated the guards and
// the extractors twice, and a guard which changed its result threw MatchError. A partial function literal evaluates them
// once, in `applyOrElse`.

import cps.*
import cps.monads.{*, given}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

object Cnt:
  var n = 0
  // true on odd calls
  def tick(): Boolean =
    n += 1
    n % 2 == 1

object Even:
  var calls = 0
  def unapply(x: Int): Option[Int] =
    calls += 1
    if x % 2 == 0 then Some(x / 2) else None

object Guard:
  var calls = 0
  def check(b: Boolean): Boolean =
    calls += 1
    b

class Pipe(xs: List[Int]):
  def collectMap(pf: PartialFunction[Int, Int], f: Int => Int): List[Int] = xs.collect(pf).map(f)
  def collectMap_async[F[_]](m: CpsMonad[F], pf: PartialFunction[Int, F[Int]], f: Int => F[Int]): F[List[Int]] =
    xs.collect(pf).foldRight(m.pure(List.empty[Int])) { (fv, acc) =>
      m.flatMap(fv)(v => m.flatMap(f(v))(r => m.map(acc)(r :: _)))
    }

def run[T](f: => Future[T]): T = Await.result(f, 5.seconds)

@main def test(): Unit =
  val sync = List(1, 2, 3).collect { case x if Cnt.tick() => x }
  val syncCalls = Cnt.n
  // guard with side effects
  Cnt.n = 0
  val r1 = run(async[Future] {
    List(1, 2, 3).collect { case x if Cnt.tick() => await(Future(x)) }
  })
  val n1 = Cnt.n
  // extractor with side effects
  Even.calls = 0
  val r2 = run(async[Future] {
    List(1, 2, 3, 4).collect { case Even(h) => await(Future(h)) }
  })
  val n2 = Even.calls
  // collectFirst
  Even.calls = 0
  val r3 = run(async[Future] {
    List(1, 2, 3, 4).collectFirst { case Even(h) if h > 1 => await(Future(h)) }
  })
  val n3 = Even.calls
  // Option.collect
  Even.calls = 0
  val r4 = run(async[Future] {
    Option(4).collect { case Even(h) => await(Future(h)) }
  })
  val n4 = Even.calls
  // Try.recover
  val failed: scala.util.Try[Int] = scala.util.Failure(new RuntimeException("e"))
  Guard.calls = 0
  val r5 = run(async[Future] {
    failed.recover { case _: RuntimeException if Guard.check(true) => await(Future(5)) }
  })
  val n5 = Guard.calls
  // sync partial function, shifted because of the async lambda
  Guard.calls = 0
  val r6 = run(async[Future] {
    Pipe(List(1, 2, 3)).collectMap({ case x if Guard.check(x % 2 == 0) => x * 10 }, y => y + await(Future(y)))
  })
  val n6 = Guard.calls
  val r = List(sync, syncCalls, r1, n1, r2, n2, r3, n3, r4, n4, r5, n5, r6, n6)
  println(r)
  assert(r == List(List(1, 3), 3, List(1, 3), 3, List(1, 2), 4, Some(2), 4, Some(2), 1, scala.util.Success(5), 1, List(40), 3))
