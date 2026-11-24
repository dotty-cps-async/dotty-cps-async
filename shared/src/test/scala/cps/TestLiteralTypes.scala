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

    // a sync scrutinee, an async case
    def selfSyncScrutinee(k: Int, r: () => Future[Int]): Future[this.type] = async[Future] {
      k match
        case 1 => { await(r()); this }
        case _ => this
    }
  }

  @Test def testThisTypeResult() = {
    val s = Self()
    val r = () => Future.successful(1)
    val c = for {
      a <- s.self(r)
      b <- s.selfSyncScrutinee(1, r)
    } yield (a eq s) && (b eq s)
    FutureCompleter(c.map(v => assert(v)))
  }

  // the right part of the assignment has type `x.type | y.type`, the variable is declared `Int`
  def assignValRefMatch[F[_]: CpsMonad](r: () => F[Int]): F[Int] =
    val x = 1
    val y = 2
    async[F] {
      var v = 0
      v = await(r()) match
        case 1 => { await(r()); x }
        case _ => y
      var w = 0
      w = await(r()) match
        case 1 => x
        case _ => y
      v + w
    }

  // the variable is declared with a type, which `me` conforms to
  class SelfAssign {
    def run(r: () => Future[Int]): Future[Boolean] =
      val me: this.type = this
      async[Future] {
        var v: SelfAssign = new SelfAssign
        v = await(r()) match
          case 1 => { await(r()); me }
          case _ => me
        v eq this
      }
  }

  // the result has type `me.type`: `me` is declared with `this.type`
  class SelfVal {
    def viaVal(r: () => Future[Int]): Future[this.type] =
      val me: this.type = this
      async[Future] {
        await(r()) match
          case 1 => { await(r()); me }
          case _ => me
      }
  }

  @Test def testThisTypeOfVals() = {
    val r = () => Future.successful(1)
    val sv = SelfVal()
    val c = for {
      a <- SelfAssign().run(r)
      b <- sv.viaVal(r)
    } yield (a, b eq sv)
    FutureCompleter(c.map(v => assert(v == (true, true), s"v=$v")))
  }

  // not supported (#179): a match with async cases for a variable of a union of singleton types of vals (the workaround: a wider
  //  type of the variable, as in assignValRefMatch)
  inline def usesCompilerPlugin: Boolean =
    scala.compiletime.summonFrom {
      case given cps.macros.flags.UseCompilerPlugin.type => true
      case _                                            => false
    }

  @Test def testSingletonTermTypesAreReported() = {
    val errors = scala.compiletime.testing.typeCheckErrors("""
      {
        val x = 1
        val y = 2
        async[Future] {
          var v: x.type | y.type = x
          v = await(g(1)) match
            case 10 => { await(g(2)); y }
            case _  => x
          v
        }
      }
    """)
    // with the compiler plugin, async is transformed after the typer: there are no errors here
    if (usesCompilerPlugin) then assert(errors.isEmpty, errors.map(_.message).mkString("\n"))
    else
      assert(
        errors.exists(_.message.contains("singleton types of terms are not supported")),
        errors.map(_.message).mkString("\n")
      )
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
    FutureCompleter(c.map(v => assert(v == (2, "a!a!", "a"), s"v=$v")))
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

  // #177: literal types in lambda bodies, by-name and varargs arguments, vals of singleton types of terms

  def lambdaLiteralResults[F[_]: CpsMonad](r: () => F[Int]): F[(List["a" | "b"], List["a" | "b"], List["a"])] = async[F] {
    val a = List(1, 2).map[("a" | "b")](x => if (await(r()) > x) "a" else "b")
    val b = List(1, 2).map { (x: Int) =>
      val v: "a" | "b" = { await(r()); "a" }
      v
    }
    val c = List(1, 2).map["a"](x => { await(r()); "a" })
    (a, b, c)
  }

  def partialFunctionLiteralResult[F[_]: CpsMonad](r: () => F[Int]): F[List["a" | "b"]] = async[F] {
    List(1, 2).collect[("a" | "b")] { case x if x > 0 => if (await(r()) > x) "a" else "b" }
  }

  def optionMapLiteralResult[F[_]: CpsMonad](r: () => F[Int], o: Option[Int]): F[Option["a" | "b"]] = async[F] {
    o.map[("a" | "b")](x => if (await(r()) > x) "a" else "b")
  }

  transparent inline def passOnA(g: Int => "a"): List["a"] = List(1, 2).map(g)

  def inlineProxyLambdaLiteralResult[F[_]: CpsMonad](r: () => F[Int]): F[List["a"]] = async[F] {
    passOnA(x => { await(r()); "a" })
  }

  @Test def testLambdaLiteralResultsInGenericMonad() = {
    val r = () => Future.successful(1)
    val c = for {
      a <- lambdaLiteralResults[Future](r)
      b <- partialFunctionLiteralResult[Future](r)
      d <- optionMapLiteralResult[Future](r, Some(0))
      e <- inlineProxyLambdaLiteralResult[Future](r)
    } yield (a, b, d, e)
    FutureCompleter(
      c.map(v =>
        assert(
          v == ((List("b", "b"), List("a", "a"), List("a", "a")), List("b", "b"), Some("a"), List("a", "a")),
          s"v=$v"
        )
      )
    )
  }

  class SelfInLambda {
    def selves[F[_]: CpsMonad](r: () => F[Int]): F[List[this.type]] = async[F] {
      List(1, 2).map[this.type](x => { await(r()); this })
    }
  }

  @Test def testThisTypeLambdaResultInGenericMonad() = {
    val s = SelfInLambda()
    val c = s.selves[Future](() => Future.successful(1))
    FutureCompleter(c.map(v => assert(v.size == 2 && v.forall(_ eq s), s"v=$v")))
  }

  def byNameLiteralArgs[F[_]: CpsMonad](r: () => F[Int], o: Option["a" | "b"]): F[("a" | "b", "a" | "b")] = async[F] {
    val a: "a" | "b" = o.getOrElse(if (await(r()) > 0) "a" else "b")
    val b: "a" | "b" = o.getOrElse({ await(r()); "b" })
    (a, b)
  }

  class SelfByName {
    def selfOrElse[F[_]: CpsMonad](r: () => F[Int], o: Option[this.type]): F[this.type] = async[F] {
      o.getOrElse({ await(r()); this })
    }
  }

  @Test def testByNameLiteralArgsInGenericMonad() = {
    val r = () => Future.successful(1)
    val s = SelfByName()
    val c = for {
      a <- byNameLiteralArgs[Future](r, None)
      b <- s.selfOrElse[Future](r, None)
    } yield (a, b eq s)
    FutureCompleter(c.map(v => assert(v == (("a", "b"), true), s"v=$v")))
  }

  def hv(xs: ("a" | "b")*): String = xs.mkString

  def hkv(k: Int, xs: ("a" | "b")*): String = k.toString + xs.mkString

  def varargsLiteralArgs[F[_]: CpsMonad](r: () => F[Int]): F[(String, String, List["a" | "b"])] = async[F] {
    val a = hv(if (await(r()) > 0) "a" else "b", "b")
    val b = hkv(await(r()), "a", if (await(r()) > 0) "b" else "a")
    val c = List[("a" | "b")](if (await(r()) > 0) "a" else "b", "b")
    (a, b, c)
  }

  @Test def testVarargsLiteralArgsInGenericMonad() = {
    val c = varargsLiteralArgs[Future](() => Future.successful(1))
    FutureCompleter(c.map(v => assert(v == ("ab", "1ab", List("a", "b")), s"v=$v")))
  }

  object Holder {
    lazy val q = 7
  }

  def valsOfTermSingletonTypes[F[_]: CpsMonad](r: () => F[Int]): F[(Int, Int, Int)] =
    val x = 1
    val y = 5
    async[F] {
      val z: y.type = { await(r()); y }
      val v: x.type | y.type = if (await(r()) > 0) x else y
      val q: Holder.q.type = { await(r()); Holder.q }
      (z + 1, v + 0, q + 1)
    }

  @Test def testValsOfTermSingletonTypesInGenericMonad() = {
    val c = valsOfTermSingletonTypes[Future](() => Future.successful(1))
    FutureCompleter(c.map(v => assert(v == (6, 1, 8), s"v=$v")))
  }

  @Test def testValOfTermSingletonTypesWithAsyncCasesIsReported() = {
    val errors = scala.compiletime.testing.typeCheckErrors("""
      {
        val x = 1
        val y = 2
        async[Future] {
          val v: x.type | y.type = await(g(1)) match
            case 10 => { await(g(2)); y }
            case _  => x
          v
        }
      }
    """)
    // with the compiler plugin, async is transformed after the typer: there are no errors here
    if (usesCompilerPlugin) then assert(errors.isEmpty, errors.map(_.message).mkString("\n"))
    else
      assert(
        errors.exists(_.message.contains("singleton types of terms are not supported")),
        errors.map(_.message).mkString("\n")
      )
  }

  @Test def testLambdaAndByNameLiteralResultsInInvariantMonad() = {
    val c: PureEffect[(List["a" | "b"], "a" | "b")] = async[PureEffect] {
      val a = List(1, 2).map[("a" | "b")](x => if (await(PureEffect.pure(x)) > 1) "a" else "b")
      val b: "a" | "b" = Option.empty["a" | "b"].getOrElse({ await(PureEffect.pure(1)); "b" })
      (a, b)
    }
    FutureCompleter(c.unsafeRunFuture().map(v => assert(v == (List("b", "a"), "b"), s"v=$v")))
  }

}
