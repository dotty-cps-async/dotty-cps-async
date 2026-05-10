package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given


class TestBS1ShiftTry:


  @Test def testTryApply(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
            Try(await(T1.cbi(2)))
     }
     val r = c.run()
     assert(c.run() == Success(Success(2)))


  @Test def testTryGetOrElse0(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val a: Try[Int] = Success(0)
        a.getOrElse{
            await(T1.cbi(2))
        }
     }
     assert(c.run() == Success(0))

  @Test def testTryRecoverMatched(): Unit =
     val c = async[ComputationBound]{
        val a: Try[Int] = Failure(new IllegalArgumentException("bad"))
        a.recover{ case _: IllegalArgumentException => await(T1.cbi(42)) }
     }
     assert(c.run() == Success(Success(42)))

  @Test def testTryRecoverUnmatchedPreservesFailure(): Unit =
     val ex = new RuntimeException("boom")
     val c = async[ComputationBound]{
        val a: Try[Int] = Failure(ex)
        a.recover{ case _: IllegalArgumentException => await(T1.cbi(42)) }
     }
     assert(c.run() == Success(Failure(ex)))

  @Test def testTryRecoverWithMatched(): Unit =
     val c = async[ComputationBound]{
        val a: Try[Int] = Failure(new IllegalArgumentException("bad"))
        a.recoverWith{ case _: IllegalArgumentException => Success(await(T1.cbi(7))) }
     }
     assert(c.run() == Success(Success(7)))

  @Test def testTryRecoverWithUnmatchedPreservesFailure(): Unit =
     val ex = new RuntimeException("boom")
     val c = async[ComputationBound]{
        val a: Try[Int] = Failure(ex)
        a.recoverWith{ case _: IllegalArgumentException => Success(await(T1.cbi(7))) }
     }
     assert(c.run() == Success(Failure(ex)))

/*
  @Test def testEitherGetOrElse1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val a: Either[String,Int] = Left("A")
        a.getOrElse{
            await(T1.cbi(2))
        }
     }
     assert(c.run() == Success(2))

  @Test def testEitherLeftForAll1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val a: Either[String,Int] = Left("A")
        a.left.forall{ _ == await(T1.cbs("A")) }
     }
     assert(c.run() == Success(true))
*/




