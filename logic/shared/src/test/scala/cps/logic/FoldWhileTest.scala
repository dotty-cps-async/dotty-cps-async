package cps.logic

import org.junit.{Ignore, Test}
import cps.*
import cps.monads.{*, given}
import cps.monads.logic.{*, given}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

class FoldWhileTest {

  @Test
  def checkLazyListFoldWhile(): Unit = {
    val m = LazyList.from(1 to 100)
    val res = m.foldWhile(0)(x => x < 10)(_ + _)
    assert(res == (1 to 4).sum)
    val res2 = m.foldWhile(0)(_ => false)(_ + _)
    assert(res2 == 1)
  }

  @Test
  def checkLogicStreamFoldWhile() = {
    //assert(false)
    val m = LogicStream.fromCollection(1 to 100)
    // 1, 2 [state=3], 3 [state 6], 4 [state=10],
    val res = m.foldWhile(0)(s => s < 10){ (s,x) =>
        s + x
    }
    assert(res == (1 to 4).sum)
    val res2 = m.foldWhile(0)(_ => false)(_ + _)
    assert(res2 == 0)
  }


}
