package cps.preprocessor

import scala.concurrent.Future
import scala.quoted._
import cps._

/**
 * Macro implementations for test preprocessors.
 * These must be compiled before the test files that use them.
 */
object TestPreprocessorMacros:

  /**
   * Preprocessor macro that marks tracking flag and returns body unchanged.
   * The ctx parameter is available but not used in this simple test.
   */
  def trackingImpl[A: Type, C: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{
      PreprocessorTracker.markCalled()
      $body
    }

  /**
   * Preprocessor macro that wraps body with counter.
   * The ctx parameter is available but not used in this simple test.
   */
  def wrappingImpl[A: Type, C: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{ PreprocessorTracker.wrap($body) }
