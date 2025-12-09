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
 * The preprocessor receives the body expression where the CpsMonadContext[F]
 * is available in scope. This allows inserting operations that use the context,
 * such as ctx.monad for accessing the monad instance.
 *
 * Use cases:
 * - Durable monad: wrap vals with caching via ctx operations
 * - Tracing monad: add trace calls
 * - STM monad: add savepoints
 *
 * Example implementation:
 * {{{
 * given CpsPreprocessor[Durable] with
 *   inline def preprocess[A](inline body: A): A =
 *     \${ DurablePreprocessMacro.impl[A]('body) }
 * }}}
 *
 * The preprocessor macro can transform val definitions, control flow conditions,
 * and other expressions as needed by the monad semantics.
 */
trait CpsPreprocessor[F[_]]:
  /**
   * Transform the body of an async block before CPS transformation.
   *
   * @param body The body expression inside the async block. The CpsMonadContext[F]
   *             is available in scope within this body, so preprocessor can insert
   *             calls like `summon[CpsMonadContext[F]].cached(...)` or access the
   *             monad via `summon[CpsMonadContext[F]].monad`.
   * @tparam A The result type of the body expression
   * @return The transformed body expression
   */
  inline def preprocess[A](inline body: A): A
