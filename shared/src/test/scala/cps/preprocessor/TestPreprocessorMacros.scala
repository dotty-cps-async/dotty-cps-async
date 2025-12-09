package cps.preprocessor

import scala.quoted._
import cps._

/**
 * Macro implementations for test preprocessors.
 * These must be in a separate file from the test usage due to Scala 3 macro rules.
 */
object TestPreprocessorMacros:

  /**
   * Preprocessor macro that marks tracking flag and returns body unchanged.
   */
  def trackingImpl[A: Type](body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{
      TestPreprocessorTracking.wasCalled = true
      $body
    }

  /**
   * Preprocessor macro that wraps body with counter.
   */
  def wrappingImpl[A: Type](body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{ TestPreprocessorWrapping.wrap($body) }
