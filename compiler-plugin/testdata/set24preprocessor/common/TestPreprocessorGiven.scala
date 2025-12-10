package cps.preprocessor

import scala.concurrent.Future
import cps._

/**
 * Provides a top-level CpsPreprocessor[Future, C] given for the tests.
 * This is needed because the compiler plugin's implicit search runs
 * at phase level and needs the given to be in scope.
 */
given [C <: CpsMonadContext[Future]]: CpsPreprocessor[Future, C] with
  inline def preprocess[A](inline body: A, inline ctx: C): A =
    ${ TestPreprocessorMacros.trackingImpl[A, C]('body, 'ctx) }
