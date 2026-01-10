package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.Success
import scala.util.Failure
import scala.util.control.TailCalls._

import cps.monads.{*, given}


class TestCBS1TailRec:

  def tailRecAwait[T](x: TailRec[T]): T = x.result

  @Test def tailrec_try_00n_p(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          x = 2
        } catch {
          case ex: Exception =>
            x = 3
        }
        x
     }
     assert(c.result == 2)

  @Test def tailrec_try_00n_f(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          throw new RuntimeException("AAA")
          x = 2
        } catch {
          case ex: Exception =>
            x = 3
        }
        x
     }
     assert(c.result == 3)

  @Test def tailrec_try_10n_p(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          x = tailRecAwait(done(2))
        } catch {
          case ex: Exception =>
            x = 3
        }
        x
     }
     assert(c.result == 2)

  @Test def tailrec_try_10n_f(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          x = tailRecAwait(done(2))
          throw new RuntimeException("AAA")
        } catch {
          case ex: Exception =>
            x = 3
        }
        x
     }
     assert(c.result == 3)

  @Test def tailrec_try_01n_p(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          x = 2
        } catch {
          case ex: Exception =>
            x = tailRecAwait(done(3))
        }
        x
     }
     assert(c.result == 2)

  @Test def tailrec_try_01n_f(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          x = 2
          throw new RuntimeException("AAA")
        } catch {
          case ex: Exception =>
            x = tailRecAwait(done(3))
        }
        x
     }
     assert(c.result == 3)

  @Test def tailrec_try_11n_p(): Unit =
     val c = async[TailRec] {
        var x = 1
        try {
          x = tailRecAwait(done(2))
        } catch {
          case ex: Exception =>
            x = tailRecAwait(done(3))
        }
        x
     }
     assert(c.result == 2)

  @Test def tailrec_try_finally_pass(): Unit =
     var finalizerRan = false
     val c = async[TailRec] {
        var x = 1
        try {
          x = tailRecAwait(done(2))
        } finally {
          finalizerRan = true
        }
        x
     }
     assert(c.result == 2)
     assert(finalizerRan)

  @Test def tailrec_try_finally_fail(): Unit =
     var finalizerRan = false
     var caughtEx: Option[Throwable] = None
     try {
       val c = async[TailRec] {
          var x = 1
          try {
            x = tailRecAwait(done(2))
            throw new RuntimeException("test error")
          } finally {
            finalizerRan = true
          }
          x
       }
       c.result
     } catch {
       case ex: RuntimeException =>
         caughtEx = Some(ex)
     }
     assert(finalizerRan)
     assert(caughtEx.isDefined)
     assert(caughtEx.get.getMessage == "test error")

  @Test def tailrec_try_catch_finally(): Unit =
     var finalizerRan = false
     val c = async[TailRec] {
        var x = 1
        try {
          x = tailRecAwait(done(2))
          throw new RuntimeException("AAA")
        } catch {
          case ex: Exception =>
            x = 3
        } finally {
          finalizerRan = true
        }
        x
     }
     assert(c.result == 3)
     assert(finalizerRan)

  @Test def tailrec_nested_try(): Unit =
     val c = async[TailRec] {
        var x = 0
        try {
          try {
            x = tailRecAwait(done(1))
            throw new RuntimeException("inner")
          } catch {
            case ex: RuntimeException if ex.getMessage == "inner" =>
              x = 2
              throw new RuntimeException("outer")
          }
        } catch {
          case ex: RuntimeException if ex.getMessage == "outer" =>
            x = 3
        }
        x
     }
     assert(c.result == 3)

  @Test def tailrec_deep_recursion_with_try(): Unit =
     // Test that TailRec properly trampolines even with try/catch
     def loop(n: Int, acc: Int): TailRec[Int] =
       if n <= 0 then done(acc)
       else tailcall(loop(n - 1, acc + 1))

     val c = async[TailRec] {
        var result = 0
        try {
          result = tailRecAwait(loop(10000, 0))
        } catch {
          case ex: StackOverflowError =>
            result = -1
        }
        result
     }
     // Should complete without StackOverflow
     assert(c.result == 10000)

