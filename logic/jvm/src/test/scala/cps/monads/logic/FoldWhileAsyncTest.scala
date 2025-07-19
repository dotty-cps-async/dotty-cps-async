package cps.logic

import org.junit.{Ignore, Test}
import cps.*
import cps.monads.{*, given}
import cps.monads.logic.{*, given}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

class FoldWhileAsyncTest {


  @Test
  def checkAsyncLogicStreamFoldWhile():Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val m: LogicStreamT[Future,Int] = all(1 to 100)
    val res = m.foldWhile(0)(x => x < 10)(_ + _)
    val next = res.flatMap { r =>
      assert(r == (1 to 4).sum)
      Future.successful(())
    }.recover { case e: Throwable =>
      println(s"Error: ${e.getMessage}")
      Future.failed(e)
    }
    Await.result(next, 1.second)
  }



}
