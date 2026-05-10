package cps

import org.junit.Test
import org.junit.Assert._

import scala.util.{Success, Failure}
import scala.util.control.Breaks

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
