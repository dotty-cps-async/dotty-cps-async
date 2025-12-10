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
   * The ctx parameter is available but not used in this simple test.
   */
  def trackingImpl[A: Type, C: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{
      TestPreprocessorTracking.wasCalled = true
      $body
    }

  /**
   * Preprocessor macro that wraps body with counter.
   * The ctx parameter is available but not used in this simple test.
   */
  def wrappingImpl[A: Type, C: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{ TestPreprocessorWrapping.wrap($body) }

  /**
   * Preprocessor macro that demonstrates using ctx.
   * Records context monad access for verification.
   */
  def withContextImpl[A: Type, C <: CpsMonadContext[ComputationBound]: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*
    '{
      TestPreprocessorWithContext.recordContextAccess[C]($ctx)
      $body
    }
