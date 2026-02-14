package cps

import org.junit.{Test, Ignore}
import org.junit.Assert._
import cps.macros.misc.ChangeOwnerTest

/**
 * Self-contained tests to find which tree manipulation breaks default getter Selects.
 */
class TestChangeOwnerDefaultGetter:

  case class P(x: Int, y: Int)

  @Test def simpleWrap(): Unit =
    val f = ChangeOwnerTest.wrapInLambda {
      val p = P(3, 4)
      p.copy(y = 7)
    }
    assertEquals(P(3, 7), f())

  @Test def simulateAsyncMacro(): Unit =
    val f = ChangeOwnerTest.simulateAsyncMacro {
      val p = P(3, 4)
      p.copy(y = 7)
    }
    assertEquals(P(3, 7), f())

  @Test def withTreeMap(): Unit =
    val f = ChangeOwnerTest.simulateWithTreeMap {
      val p = P(3, 4)
      p.copy(y = 7)
    }
    assertEquals(P(3, 7), f())
