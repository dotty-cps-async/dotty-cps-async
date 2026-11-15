package cps

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given
import cps.monads.given
import cps.pe.{PureEffect, given}
import cps.util.FutureCompleter

/** Literal types (and unions of them) and `this.type` of terms with `await` are not widened, when the expected type requires them: a
  * union written by the user (`val t: "a" | "b"`) can't be told from a union of branches (#166). Without such an expected type, they are
  * widened as before. Singleton types of vals (also in unions: `if c then x else y` has type `x.type | y.type`) are widened, as the
  * transformed terms do not keep them.
  */
class TestLiteralTypes {

  def g(x: Int): Future[Int] = Future.successful(x * 10)

  def h(x: "a" | "b"): String = "h" + x

  @Test def testValWithLiteralUnionType() = {
    val c = async[Future] {
      val t: "a" | "b" = { await(g(1)); "b" }
      t == "b"
    }
    FutureCompleter(c.map(r => assert(r)))
  }

  @Test def testLiteralUnionResult() = {
    val c = async[Future] {
      val t: "a" | "b" = { await(g(1)); "b" }
      t
    }
    val typed: Future["a" | "b"] = c
    FutureCompleter(typed.map(r => assert(r == "b", s"r=$r")))
  }

  @Test def testLiteralResult() = {
    val c: Future["b"] = async[Future] { await(g(1)); "b" }
    FutureCompleter(c.map(r => assert(r == "b", s"r=$r")))
  }

  @Test def testLiteralUnionArgument() = {
    val c = async[Future] {
      h(if (await(g(1)) > 5) "a" else "b")
    }
    FutureCompleter(c.map(r => assert(r == "ha", s"r=$r")))
  }

  @Test def testLiteralUnionMatch() = {
    val c = async[Future] {
      val x = await(g(1))
      val t: "a" | "b" = x match
        case 10 => { await(g(2)); "a" }
        case _  => "b"
      t
    }
    FutureCompleter(c.map(r => assert(r == "a", s"r=$r")))
  }

  @Test def testLiteralUnionValInBranch() = {
    val c = async[Future] {
      val t: "a" | "b" = { await(g(1)); "a" }
      val r: "a" | "b" = if (await(g(1)) > 5) { await(g(2)); t } else "b"
      r
    }
    FutureCompleter(c.map(r => assert(r == "a", s"r=$r")))
  }

  @Test def testValRefUnionMatch() = {
    val x = 1
    val y = 2
    val c = async[Future] {
      // the type of the match is `x.type | y.type`
      await(g(1)) match
        case 10 => { await(g(2)); x }
        case _  => y
    }
    FutureCompleter(c.map(r => assert(r == 1, s"r=$r")))
  }

  // the last expression of the block is ascribed by the compiler as `(... match ...): Int`, the match has type `y.type | z.type`
  def localValRefUnion[F[_]: CpsMonad](r: () => F[Int]): F[Int] = async[F] {
    val y = 3
    val z = 4
    await(r()) match
      case 1 => { await(r()); y }
      case _ => z
  }

  @Test def testLocalValRefUnionInGenericMonad() = {
    val c = localValRefUnion[Future](() => Future.successful(1))
    FutureCompleter(c.map(r => assert(r == 3, s"r=$r")))
  }

  def literalCasesMatch[F[_]: CpsMonad](r: () => F[Option[Int]], acc: Array[Int]): F[Boolean] = async[F] {
    await(r()) match
      case Some(a) =>
        acc(0) += await(r()).getOrElse(0)
        true
      case None =>
        false
  }

  @Test def testLiteralCasesInGenericMonad() = {
    val acc = Array(0)
    val c = literalCasesMatch[Future](() => Future.successful(Some(2)), acc)
    FutureCompleter(c.map(r => assert(r && acc(0) == 2, s"r=$r, acc=${acc(0)}")))
  }

  // the value of the match is discarded

  def valRefUnionStatement[F[_]: CpsMonad](r: () => F[Int]): F[Int] =
    val x = 1
    val y = 2
    async[F] {
      await(r()) match
        case 1 => { await(r()); x }
        case _ => y
      0
    }

  def literalUnionStatement[F[_]: CpsMonad](r: () => F[Int]): F[Int] = async[F] {
    await(r()) match
      case 1 => { await(r()); "x" }
      case _ => "y"
    0
  }

  @Test def testStatementsInGenericMonad() = {
    val r = () => Future.successful(1)
    val c = for {
      a <- valRefUnionStatement[Future](r)
      b <- literalUnionStatement[Future](r)
    } yield (a, b)
    FutureCompleter(c.map(v => assert(v == (0, 0), s"v=$v")))
  }

  def singleLiteralMatch[F[_]: CpsMonad](r: () => F[Int]): F["x"] = async[F] {
    await(r()) match
      case 1 => { await(r()); "x" }
      case _ => "x"
  }

  @Test def testSingleLiteralMatchInGenericMonad() = {
    val c = singleLiteralMatch[Future](() => Future.successful(1))
    FutureCompleter(c.map(r => assert(r == "x", s"r=$r")))
  }

  transparent inline def twiceAB(x: "a" | "b"): String = x + x

  def inlineLiteralUnionParam[F[_]: CpsMonad](r: () => F[Int]): F[String] = async[F] {
    twiceAB(if (await(r()) > 0) "a" else "b")
  }

  @Test def testInlineLiteralUnionParamInGenericMonad() = {
    val c = inlineLiteralUnionParam[Future](() => Future.successful(1))
    FutureCompleter(c.map(r => assert(r == "aa", s"r=$r")))
  }

  // without an expected type, which requires a literal type, literal types are widened

  transparent inline def twice(x: String): String = x + x

  def inlineArg[F[_]: CpsMonad](r: () => F[Int]): F[String] = async[F] {
    twice({ await(r()); "b" })
  }

  def lambdaResults[F[_]: CpsMonad](r: () => F[Int]): F[(List[Int], List[String], List[Boolean])] = async[F] {
    val a = List(1, 2).map(x => if (await(r()) > 0) 1 else 2)
    val b = List(1, 2).map(x => { await(r()); "b" })
    val c = List(1, 2).map(x =>
      x match
        case 1 => { await(r()); true }
        case _ => false
    )
    (a, b, c)
  }

  def tag(i: Int): "b" = "b"

  def foo(s: String, i: Int): String = s + i

  def precalcLiteralArg[F[_]: CpsMonad](r: () => F[Int]): F[String] = async[F] {
    foo(tag(await(r())), await(r()))
  }

  @Test def testWidenedLiteralsInGenericMonad() = {
    val r = () => Future.successful(1)
    val c = for {
      a <- inlineArg[Future](r)
      b <- lambdaResults[Future](r)
      c <- precalcLiteralArg[Future](r)
    } yield (a, b, c)
    FutureCompleter(c.map(v => assert(v == ("bb", (List(1, 1), List("b", "b"), List(true, false)), "b1"), s"v=$v")))
  }

  // `this.type` and literal types of term refs, which the expected type requires, are kept; a match without async cases
  //  has its expected type

  class Self {
    def self(r: () => Future[Int]): Future[this.type] = async[Future] {
      await(r()) match
        case 1 => this
        case _ => this
    }
  }

  @Test def testThisTypeResult() = {
    val s = Self()
    val c = s.self(() => Future.successful(1))
    FutureCompleter(c.map(r => assert(r eq s)))
  }

  // the right part of the assignment has type `x.type | y.type`
  def assignValRefMatch[F[_]: CpsMonad](r: () => F[Int]): F[Int] =
    val x = 1
    val y = 2
    async[F] {
      var v = 0
      v = await(r()) match
        case 1 => x
        case _ => y
      v
    }

  def litA: "a" = "a"

  def onlyA(s: "a"): String = s + "!"

  def termRefsOfLiteralType[F[_]: CpsMonad](r: () => F[Int]): F[String] =
    var x: "a" = "a"
    async[F] {
      onlyA({ await(r()); litA }) + onlyA({ await(r()); x })
    }

  def matchOfLiteralTypeVar[F[_]: CpsMonad](r: () => F[Int]): F["a"] =
    var x: "a" = "a"
    async[F] {
      await(r()) match
        case 1 => { await(r()); x }
        case _ => x
    }

  @Test def testSingletonTypesInGenericMonad() = {
    val r = () => Future.successful(1)
    val c = for {
      a <- assignValRefMatch[Future](r)
      c <- termRefsOfLiteralType[Future](r)
      d <- matchOfLiteralTypeVar[Future](r)
    } yield (a, c, d)
    FutureCompleter(c.map(v => assert(v == (1, "a!a!", "a"), s"v=$v")))
  }

  // PureEffect is invariant: the literal types of branches are cast to the result type

  @Test def testSoftLiteralUnionInInvariantMonad() = {
    val c = async[PureEffect] {
      if (await(PureEffect.pure(1)) > 0) { await(PureEffect.pure(2)); 1 } else 2
    }
    val typed: PureEffect[Int] = c
    FutureCompleter(typed.unsafeRunFuture().map(r => assert(r == 1, s"r=$r")))
  }

  @Test def testLiteralResultInInvariantMonad() = {
    val c = async[PureEffect] {
      val x = await(PureEffect.pure(1))
      x match
        case 1 => { await(PureEffect.pure(2)); "x" }
        case _ => "y"
    }
    val typed: PureEffect[String] = c
    FutureCompleter(typed.unsafeRunFuture().map(r => assert(r == "x", s"r=$r")))
  }

  @Test def testLiteralUnionValInInvariantMonad() = {
    val c = async[PureEffect] {
      val t: "a" | "b" = { await(PureEffect.pure(1)); "b" }
      t
    }
    val typed: PureEffect["a" | "b"] = c
    FutureCompleter(typed.unsafeRunFuture().map(r => assert(r == "b", s"r=$r")))
  }

}
