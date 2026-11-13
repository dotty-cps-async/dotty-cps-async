package cps.util

import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

// for use futures in jvm and js test.
// On JS a JUnit test is awaited only when the test method returns a Future: the result here is Future[Try[T]], and a Failure
// fails the test. So a test which ends with `FutureCompleter(...)` should not declare the `: Unit` result type
// (`@Test def testX() = ...`), otherwise on JS it is reported as passed before the future is completed.
object FutureCompleter
{

  def apply[T](f: Future[T])(using ec:ExecutionContext): Future[Try[T]] = 
    f.transformWith( r => Future successful r )

}
