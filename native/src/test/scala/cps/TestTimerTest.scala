package cps

import org.junit.Test

import cps.util.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

class TestTimerTest {

  @Test
  def testTimer(): Unit = {
    val timer = TestTimer
    val start = System.currentTimeMillis()
    val duration = 1.second
    val future = timer.delay(duration)
    val next = future.map(_ => {
      val end = System.currentTimeMillis()
      assert(end - start >= duration.toMillis)
    })
    Await.result(next, 30.seconds)
  }

}
