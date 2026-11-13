package cps

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.Success
import scala.util.Failure

import cps.testconfig.given

object TestCBS3HoFunNoSubst {

  sealed trait MyList[+T] {
  
      def map[S](f: T=>S): MyList[S]

      def headOption: Option[T]

      def headTail: Option[MyList[T]]

      def +:[S >: T](s:S): MyList[S] =
         MyCons(s,this)

  }

  case object MyNil extends MyList[Nothing] {

      def map[S](f: Nothing=>S): MyList[S] = this

      def headOption: Option[Nothing] = None

      def headTail: Option[MyList[Nothing]] = None

  }

  case class MyCons[T](h: T, t:MyList[T]) extends MyList[T] {

      def map[S](f: T=>S): MyList[S] = 
         MyCons(f(h),t.map(f))

      def headOption: Option[T] = Some(h)
      
      def headTail: Option[MyList[T]] = Some(t)

  }

  def  twice(x:Int)(f: Int => Int):Int =
     f(f(x))


  def  twice2(x:Int, f: Int => Int):Int =
     f(f(x))

  // no shifted variant: an async partial function argument is passed with runtime await
  def collectFrom(x: Int)(pf: PartialFunction[Int, Int]): List[Int] =
     List(x, x + 1, x + 2).collect(pf)

  def collectMap(x: Int)(pf: PartialFunction[Int, Int], f: Int => Int): List[Int] =
     List(x, x + 1, x + 2).collect(pf).map(f)



} 


class TestCBS3HoFunNoSubst:

  import TestCBS3HoFunNoSubst.*


  
  @Test def testTwice(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val y = twice(12)(x => x + await(T1.cbi(x)))
        y
     }
     c.run() match 
        case Success(y) => assert( y == 48 )
        case _ => assert( "" == "shoud be seq from 4 elements") 
   

  @Test def testTwice2(): Unit = 
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val c = async[ComputationBound]{
        val y = twice2(1,x => x + await(T1.cbi(x)))
        y
     }
     c.run() match 
        case Success(y) => assert( y == 4 )
        case _ => assert( "" == "shoud be seq from 4 elements") 
    


  // https://github.com/dotty-cps-async/dotty-cps-async/issues/155
  @Test def testPartialFunction(): Unit =
     var guardCalls = 0
     val c = async[ComputationBound]{
        collectFrom(1){ case x if { guardCalls += 1; x % 2 == 0 } => x + await(T1.cbi(x)) }
     }
     val r = c.run()
     assert(r == Success(List(4)), s"r=$r")
     assert(guardCalls == 3, s"guardCalls=$guardCalls")

  // sync partial function, passed with runtime await because of the async lambda
  @Test def testSyncPartialFunctionWithAsyncLambda(): Unit =
     val c = async[ComputationBound]{
        collectMap(1)({ case x if x % 2 == 0 => x * 10 }, y => y + await(T1.cbi(y)))
     }
     val r = c.run()
     assert(r == Success(List(40)), s"r=$r")

  @Test def testMyMap(): Unit =
     val c = async[ComputationBound]{
        val l1 =  1 +: 2 +: 3 +: 4 +: 5 +: MyNil
        l1.map( x =>  x + await(T1.cbi(1)))       
     }
     val r = c.run()
     assert(r.isSuccess)
     assert(r.get.headOption == Some(2))

     

