package cps

import org.junit.Test
import org.junit.Assert._

import scala.util.{Success, Failure}
import scala.util.control.{Breaks, NonFatal}

import cps.testconfig.given


class TestBS1ShiftBreaks:

  @Test def testBreakableRunsBody(): Unit =
     var seen = 0
     val c = async[ComputationBound]{
        Breaks.breakable {
           seen = await(T1.cbi(1))
           seen += await(T1.cbi(2))
        }
     }
     assert(c.run() == Success(()))
     assertEquals(3, seen)

  @Test def testBreakablePropagatesOtherExceptions(): Unit =
     val c = async[ComputationBound]{
        Breaks.breakable {
           val x = await(T1.cbi(1))
           throw new RuntimeException("not a break")
        }
     }
     c.run() match
       case Failure(ex) => assertEquals("not a break", ex.getMessage)
       case other       => assert(false, s"expected Failure, got $other")

  @Test def testBreakableHonoursSynchronousBreak(): Unit =
     var beforeBreak = 0
     var afterBreak = 0
     val c = async[ComputationBound]{
        Breaks.breakable {
           beforeBreak = 7
           if beforeBreak == 7 then Breaks.break()
           afterBreak = await(T1.cbi(99))
        }
     }
     assert(c.run() == Success(()))
     assertEquals(7, beforeBreak)
     assertEquals(0, afterBreak)

  @Test def testBreakableHonoursBreak(): Unit =
     var beforeBreak = 0
     var afterBreak = 0
     val c = async[ComputationBound]{
        Breaks.breakable {
           beforeBreak = await(T1.cbi(7))
           Breaks.break()
           afterBreak = await(T1.cbi(99))
        }
     }
     assert(c.run() == Success(()))
     assertEquals(7, beforeBreak)
     assertEquals(0, afterBreak)

  @Test def testBreakableMultipleAwaitsBeforeBreak(): Unit =
     var sumBefore = 0
     var sumAfter = 0
     val c = async[ComputationBound]{
        Breaks.breakable {
           sumBefore += await(T1.cbi(1))
           sumBefore += await(T1.cbi(2))
           Breaks.break()
           sumAfter += await(T1.cbi(3))
           sumAfter += await(T1.cbi(4))
        }
     }
     assert(c.run() == Success(()))
     assertEquals(3, sumBefore)
     assertEquals(0, sumAfter)

  @Test def testUserNonFatalCatchInsideBodyDoesNotSwallowBreak(): Unit =
     var caught = false
     var afterBreak = 0
     val c = async[ComputationBound]{
        Breaks.breakable {
           val v = await(T1.cbi(1))
           try {
              Breaks.break()
           } catch {
              case NonFatal(_) => caught = true
           }
           afterBreak = await(T1.cbi(99))
        }
     }
     assert(c.run() == Success(()))
     assertEquals(false, caught)
     assertEquals(0, afterBreak)
