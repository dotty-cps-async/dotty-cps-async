package cpstest

import org.junit.Test
import org.junit.Assert._

import scala.util.*
import scala.util.control.Breaks
import scala.util.control.NonLocalReturns.*

import cps.*
import cps.testconfig.given

case class NPPerson(name: String, age: Int)
case class NPPair(first: NPPerson, second: NPPerson)

// named patterns: `case C(name = p)`
class TestNamedPatterns {

  @Test def testNamedPattern(): Unit = {
    val c = async[ComputationBound] {
      NPPerson("a", 5) match
        case NPPerson(age = a) => a + 1
    }
    assert(c.run() == Success(6))
  }

  @Test def testNamedPatternAwaitInScrutinee(): Unit = {
    val c = async[ComputationBound] {
      await(T1.cbt(NPPerson("b", 5))) match
        case NPPerson(age = a) => a + 1
    }
    assert(c.run() == Success(6))
  }

  @Test def testNamedPatternAwaitInCase(): Unit = {
    val c = async[ComputationBound] {
      NPPerson("c", 5) match
        case NPPerson(age = a, name = n) => n + (a + await(T1.cbi(1)))
    }
    assert(c.run() == Success("c6"))
  }

  @Test def testNestedNamedPatterns(): Unit = {
    val c = async[ComputationBound] {
      await(T1.cbt(NPPair(NPPerson("a", 1), NPPerson("b", 2)))) match
        case NPPair(second = NPPerson(name = "a"))                                        => -1
        case NPPair(first = NPPerson(age = a), second = p @ NPPerson(name = "b" | "c")) => a + p.age + await(T1.cbi(10))
        case _                                                                            => 0
    }
    assert(c.run() == Success(13))
  }

  @Test def testNamedPatternInShiftedLambda(): Unit = {
    val c = async[ComputationBound] {
      List(NPPerson("a", 1), NPPerson("b", 2)).map { case NPPerson(age = a) =>
        a + await(T1.cbi(10))
      }
    }
    assert(c.run() == Success(List(11, 12)))
  }

  @Test def testNamedPatternInBreakable(): Unit = {
    var r = 0
    val c = async[ComputationBound] {
      Breaks.breakable {
        await(T1.cbt(NPPerson("a", 7))) match
          case NPPerson(age = a) =>
            r = a
            if (a > 5) Breaks.break()
        r = -1
      }
    }
    assert(c.run() == Success(()))
    assertEquals(7, r)
  }

  @Test def testNamedPatternInReturning(): Unit = {
    val c = async[ComputationBound] {
      returning {
        await(T1.cbt(NPPerson("a", 7))) match
          case NPPerson(age = a) if a > 5 => throwReturn(a)
          case _                          =>
        0
      }
    }
    assert(c.run() == Success(7))
  }

}
