package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given


class TestBS1Option:

  @Test def optionGetOrElse_0(): Unit = 
     val c = async{
        var x = 1
        val y: Option[Int] = Some(1)
        var z = y.getOrElse({ x=x+1; await(T1.cbi(2)) })
        x
     }
     assert(c.run() == Success(1))

  @Test def optionGetOrElse_1(): Unit = 
     val c = async{
        var x = 1
        val y: Option[Int] = None
        var z = y.getOrElse({ x=x+1; await(T1.cbi(2)) })
        x
     }
     assert(c.run() == Success(2))

  @Test def optionGetOrElse_2_lv(): Unit =
     val c = async{
        var x = 1
        val y: Option[Int] = None
        var z = y.getOrElse{
          val x1 = x + 1;
          x=x1;
          await(T1.cbi(2))
        }
        x
     }
     assert(c.run() == Success(2))

  @Test def optionExists_some_true(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.exists(x => await(T1.cbi(x)) == 2)
     }
     assert(c.run() == Success(true))

  @Test def optionExists_some_false(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.exists(x => await(T1.cbi(x)) == 100)
     }
     assert(c.run() == Success(false))

  @Test def optionExists_none(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = None
        y.exists(x => await(T1.cbi(x)) == 2)
     }
     assert(c.run() == Success(false))

  @Test def optionForall_some_true(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.forall(x => await(T1.cbi(x)) == 2)
     }
     assert(c.run() == Success(true))

  @Test def optionForall_some_false(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.forall(x => await(T1.cbi(x)) == 100)
     }
     assert(c.run() == Success(false))

  @Test def optionForall_none(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = None
        y.forall(x => await(T1.cbi(x)) == 2)
     }
     assert(c.run() == Success(true))

  @Test def optionFind_some_true(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.find(x => await(T1.cbi(x)) == 2)
     }
     assert(c.run() == Success(Some(2)))

  @Test def optionFind_some_false(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.find(x => await(T1.cbi(x)) == 100)
     }
     assert(c.run() == Success(None))

  @Test def optionFind_none(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = None
        y.find(x => await(T1.cbi(x)) == 2)
     }
     assert(c.run() == Success(None))

  @Test def optionCollect_some_defined(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.collect{ case x if x > 0 => await(T1.cbi(x)) * 10 }
     }
     assert(c.run() == Success(Some(20)))

  @Test def optionCollect_some_notDefined(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.collect{ case x if x > 100 => await(T1.cbi(x)) * 10 }
     }
     assert(c.run() == Success(None))

  @Test def optionCollect_none(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = None
        y.collect{ case x if x > 0 => await(T1.cbi(x)) * 10 }
     }
     assert(c.run() == Success(None))

  @Test def optionFold_some(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = Some(2)
        y.fold(await(T1.cbi(0)))(x => await(T1.cbi(x)) * 10)
     }
     assert(c.run() == Success(20))

  @Test def optionFold_none(): Unit =
     val c = async[ComputationBound]{
        val y: Option[Int] = None
        y.fold(await(T1.cbi(0)))(x => await(T1.cbi(x)) * 10)
     }
     assert(c.run() == Success(0))

