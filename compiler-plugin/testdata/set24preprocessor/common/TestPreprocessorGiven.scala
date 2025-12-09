package cps.preprocessor

import scala.concurrent.Future
import cps._

/**
 * Provides a top-level CpsPreprocessor[Future] given for the tests.
 * This is needed because the compiler plugin's implicit search runs
 * at phase level and needs the given to be in scope.
 */
given testCpsPreprocessorFuture: CpsPreprocessor[Future] with
  inline def preprocess[A](inline body: A): A =
    ${ TestPreprocessorMacros.trackingImpl[A]('body) }
