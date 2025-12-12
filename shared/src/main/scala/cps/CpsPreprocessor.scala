/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021, 2022, 2023, 2024, 2025
 */
package cps

/**
 * Typeclass for preprocessing expressions inside async blocks
 * before CPS transformation occurs.
 *
 * The preprocessor receives the body expression and the monad context.
 * The context is passed explicitly because the body has type A (not a context function),
 * so the context cannot be summoned inside the body.
 *
 * Use cases:
 * - Durable monad: wrap vals with caching via ctx operations
 * - Tracing monad: add trace calls
 * - STM monad: add savepoints
 *
 * Example implementation:
 * {{{
 * given CpsPreprocessor[Durable, DurableContext] with
 *   inline def preprocess[A](inline body: A)(inline ctx: DurableContext): A =
 *     \${ DurablePreprocessMacro.impl[A]('body, 'ctx) }
 * }}}
 *
 * The preprocessor macro can transform val definitions, control flow conditions,
 * and other expressions as needed by the monad semantics, using ctx directly.
 *
 * @tparam F The monad type
 * @tparam C The context type (subtype of CpsMonadContext[F])
 */
trait CpsPreprocessor[F[_], C <: CpsMonadContext[F]]:
  /**
   * Transform the body of an async block before CPS transformation.
   *
   * @param body The body expression inside the async block
   * @param ctx The monad context, passed explicitly for use in generated code
   * @tparam A The result type of the body expression
   * @return The transformed body expression
   */
  transparent inline def preprocess[A](inline body: A, inline ctx: C): A
