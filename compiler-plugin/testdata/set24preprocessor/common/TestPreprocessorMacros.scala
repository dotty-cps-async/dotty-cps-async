package cps.preprocessor

import scala.quoted._
import cps._

/**
 * Macro implementations for test preprocessors.
 * These must be compiled before the test files that use them.
 */
object TestPreprocessorMacros:

  /**
   * Preprocessor macro that marks tracking flag and returns body unchanged.
   */
  def trackingImpl[A: Type](body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{
      PreprocessorTracker.markCalled()
      $body
    }

  /**
   * Preprocessor macro that wraps body with counter.
   */
  def wrappingImpl[A: Type](body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{ PreprocessorTracker.wrap($body) }
