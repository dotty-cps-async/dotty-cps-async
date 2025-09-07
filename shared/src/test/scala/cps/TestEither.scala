package cps

import org.junit.{Test, Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given
import cps.monads.{*, given}

class TestEither {

  // Custom error type for testing EitherCpsMonad
  case class MyError(message: String)

  // Test EitherCpsMonad basic operations
  @Test def testEitherCpsMonadPure(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      42
    }
    assert(result == Right(42))
  }

  @Test def testEitherCpsMonadMap(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      val x = await(Right(10): Either[MyError, Int])
      x * 2
    }
    assert(result == Right(20))
  }

  @Test def testEitherCpsMonadMapWithLeft(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      val x = await(Left(MyError("test error")): Either[MyError, Int])
      x * 2
    }
    assert(result == Left(MyError("test error")))
  }

  @Test def testEitherCpsMonadFlatMap(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      val x = await(Right(10): Either[MyError, Int])
      val y = await(Right(5): Either[MyError, Int])
      x + y
    }
    assert(result == Right(15))
  }

  @Test def testEitherCpsMonadFlatMapWithError(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      val x = await(Right(10): Either[MyError, Int])
      val y = await(Left(MyError("error in y")): Either[MyError, Int])
      x + y
    }
    assert(result == Left(MyError("error in y")))
  }

  @Test def testEitherCpsMonadErrorPropagation(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      val x = await(Left(MyError("first error")): Either[MyError, Int])
      val y = await(Left(MyError("second error")): Either[MyError, Int])
      x + y
    }
    assert(result == Left(MyError("first error")))
  }

  // Test EitherCpsMonad with thrown exceptions (should propagate as exceptions, not Left)
  @Test def testEitherCpsMonadThrownExceptionPropagates(): Unit = {
    var exceptionThrown = false
    try {
      val result = async[[A] =>> Either[MyError, A]] {
        throw new RuntimeException("test exception")
        42
      }
    } catch {
      case _: RuntimeException => exceptionThrown = true
    }
    assert(exceptionThrown)
  }

  // Test EitherCpsTryMonad basic operations
  @Test def testEitherCpsTryMonadPure(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      42
    }
    assert(result == Right(42))
  }

  @Test def testEitherCpsTryMonadMap(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val x = await(Right(10): Either[RuntimeException, Int])
      x * 2
    }
    assert(result == Right(20))
  }

  @Test def testEitherCpsTryMonadMapWithLeft(): Unit = {
    val error = new RuntimeException("test error")
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val x = await(Left(error): Either[RuntimeException, Int])
      x * 2
    }
    assert(result == Left(error))
  }

  @Test def testEitherCpsTryMonadFlatMap(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val x = await(Right(10): Either[RuntimeException, Int])
      val y = await(Right(5): Either[RuntimeException, Int])
      x + y
    }
    assert(result == Right(15))
  }

  @Test def testEitherCpsTryMonadFlatMapWithError(): Unit = {
    val error = new RuntimeException("error in y")
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val x = await(Right(10): Either[RuntimeException, Int])
      val y = await(Left(error): Either[RuntimeException, Int])
      x + y
    }
    assert(result == Left(error))
  }

  // Test EitherCpsTryMonad exception handling (exceptions caught and converted to Left)
  @Test def testEitherCpsTryMonadExceptionInMap(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val x = await(Right(10): Either[RuntimeException, Int])
      if (x > 5) throw new RuntimeException("test exception")
      x * 2
    }
    result match {
      case Left(ex: RuntimeException) => assert(ex.getMessage == "test exception")
      case _                          => fail("Expected Left with RuntimeException")
    }
  }

  @Test def testEitherCpsTryMonadExceptionInFlatMap(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val x = await(Right(10): Either[RuntimeException, Int])
      val y = await({
        if (x > 5) throw new RuntimeException("flatmap exception")
        Right(x * 2): Either[RuntimeException, Int]
      })
      y
    }
    result match {
      case Left(ex: RuntimeException) => assert(ex.getMessage == "flatmap exception")
      case _                          => fail("Expected Left with RuntimeException")
    }
  }

  // Test exception mapping with different exception types
  @Test def testEitherCpsTryMonadWithIllegalArgumentException(): Unit = {
    val result = async[[A] =>> Either[IllegalArgumentException, A]] {
      val x = await(Right(10): Either[IllegalArgumentException, Int])
      if (x > 5) throw new IllegalArgumentException("illegal arg")
      x * 2
    }
    result match {
      case Left(ex: IllegalArgumentException) => assert(ex.getMessage == "illegal arg")
      case _                                  => fail("Expected Left with IllegalArgumentException")
    }
  }

  // Test that non-matching exceptions are re-thrown
  @Test def testEitherCpsTryMonadNonMatchingException(): Unit = {
    var exceptionThrown = false
    try {
      val result = async[[A] =>> Either[IllegalArgumentException, A]] {
        val x = await(Right(10): Either[IllegalArgumentException, Int])
        if (x > 5) throw new RuntimeException("non-matching exception")
        x * 2
      }
    } catch {
      case _: RuntimeException => exceptionThrown = true
    }
    assert(exceptionThrown)
  }

  // Test flatMapTry functionality
  @Test def testEitherCpsTryMonadFlatMapTryWithSuccess(): Unit = {
    given monad: EitherCpsTryMonad[RuntimeException] = new EitherCpsTryMonad[RuntimeException]
    val result: Either[RuntimeException, Int] = monad.flatMapTry(Right(42)) {
      case Success(value) => Right(value * 2)
      case Failure(ex)    => Left(ex.asInstanceOf[RuntimeException])
    }
    assert(result == Right(84))
  }

  @Test def testEitherCpsTryMonadFlatMapTryWithFailure(): Unit = {
    given monad: EitherCpsTryMonad[RuntimeException] = new EitherCpsTryMonad[RuntimeException]
    val error = new RuntimeException("test error")
    val result: Either[RuntimeException, Int] = monad.flatMapTry(Left(error)) {
      case Success(value: Int) => Right(value * 2)
      case Failure(ex)         => Right(999) // converting failure to success
    }
    assert(result == Right(999))
  }

  @Test def testEitherCpsTryMonadFlatMapTryWithFailurePassThrough(): Unit = {
    given monad: EitherCpsTryMonad[RuntimeException] = new EitherCpsTryMonad[RuntimeException]
    val error = new RuntimeException("test error")
    val result: Either[RuntimeException, Int] = monad.flatMapTry(Left(error)) {
      case Success(value: Int) => Right(value * 2)
      case Failure(ex)         => Left(ex.asInstanceOf[RuntimeException])
    }
    result match {
      case Left(ex: RuntimeException) => assert(ex.getMessage == "test error")
      case _                          => fail("Expected Left with RuntimeException")
    }
  }

  // Test flatMapTry with non-Throwable error (should convert to Left without calling flatMapTry function)
  case class NonThrowableError(msg: String)

  @Test def testEitherCpsTryMonadFlatMapTryWithNonThrowableError(): Unit = {
    // Create a custom ThrowableMapping that doesn't map NonThrowableError
    given ThrowableMapping[NonThrowableError] with {
      def toThrowable(e: NonThrowableError): Option[Throwable] = None
      def fromThrowable(t: Throwable): Option[NonThrowableError] = None
    }

    given monad: EitherCpsTryMonad[NonThrowableError] = new EitherCpsTryMonad[NonThrowableError]
    val error = NonThrowableError("non-throwable error")
    val result: Either[NonThrowableError, Int] = monad.flatMapTry(Left(error)) {
      case Success(value: Int) => Right(value * 2)
      case Failure(ex) =>
        fail("This should not be called for non-throwable errors")
        Left(NonThrowableError("should not happen"))
    }
    assert(result == Left(error))
  }

  // Test more complex async/await scenarios
  @Test def testEitherCpsMonadComplexAsyncAwait(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      var sum = 0
      for (i <- 1 to 3) {
        val value = await(Right(i): Either[MyError, Int])
        sum += value
      }
      sum
    }
    assert(result == Right(6))
  }

  @Test def testEitherCpsMonadAsyncAwaitWithEarlyReturn(): Unit = {
    val result = async[[A] =>> Either[MyError, A]] {
      val a = await(Right(1): Either[MyError, Int])
      val b = await(Left(MyError("early error")): Either[MyError, Int])
      val c = await(Right(3): Either[MyError, Int]) // should not be evaluated
      a + b + c
    }
    assert(result == Left(MyError("early error")))
  }

  @Test def testEitherCpsTryMonadComplexAsyncAwait(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      var product = 1
      for (i <- 1 to 4) {
        val value = await(Right(i): Either[RuntimeException, Int])
        if (value == 4) throw new RuntimeException("stop at 4")
        product *= value
      }
      product
    }
    result match {
      case Left(ex: RuntimeException) => assert(ex.getMessage == "stop at 4")
      case _                          => fail("Expected Left with RuntimeException")
    }
  }

  @Test def testEitherCpsTryMonadAsyncAwaitWithRecovery(): Unit = {
    val result = async[[A] =>> Either[RuntimeException, A]] {
      val a = await(Right(10): Either[RuntimeException, Int])
      val b =
        try {
          if (a > 5) throw new RuntimeException("intentional error")
          a * 2
        } catch {
          case _: RuntimeException => -1 // recovery value
        }
      b
    }
    assert(result == Right(-1))
  }

}
