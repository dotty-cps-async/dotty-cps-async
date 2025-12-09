# CpsPreprocessor - Expression Preprocessing for Async Blocks

## Overview

`CpsPreprocessor[F[_]]` is a typeclass that allows monads to transform expressions inside `async` blocks before CPS (Continuation-Passing Style) transformation occurs. This enables monad-specific AST transformations such as:

- **Durable monad**: Wrap all `val` definitions with caching calls for replay-based execution
- **Tracing monad**: Insert trace/logging calls around expressions
- **STM monad**: Add savepoint markers for transactional memory

## API

```scala
package cps

trait CpsPreprocessor[F[_]]:
  /**
   * Transform the body of an async block before CPS transformation.
   *
   * @param body The body expression inside the async block. The CpsMonadContext[F]
   *             is available in scope, so preprocessor can access it via
   *             `summon[CpsMonadContext[F]]`.
   * @tparam A The result type of the body expression
   * @return The transformed body expression
   */
  inline def preprocess[A](inline body: A): A
```

## Usage

### Defining a Preprocessor

A preprocessor is typically implemented as a macro that transforms the AST:

```scala
import cps.*
import scala.quoted.*

given CpsPreprocessor[MyMonad] with
  inline def preprocess[A](inline body: A): A =
    ${ MyPreprocessorMacro.impl[A]('body) }

object MyPreprocessorMacro:
  def impl[A: Type](body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    // Transform the AST
    // The CpsMonadContext[MyMonad] is available in scope within body
    // You can insert calls like: summon[CpsMonadContext[MyMonad]].doSomething()
    body // Return transformed expression
```

### How It Works

The preprocessor is applied automatically in two transformation paths:

#### 1. Macro Path (`async[F] { }`)

When using `async[F] { body }`, the preprocessor is applied inside `transformContextLambdaImpl`:

```
async[MyMonad] { body }
       ↓
extractLambda gets (params, body)
       ↓
Summon CpsPreprocessor[MyMonad]?
       ↓ Yes
preprocessor.preprocess(body) → preprocessedBody
       ↓
CPS transformation on preprocessedBody
```

#### 2. Direct Style Path (`def foo(using CpsDirect[F])`)

For direct style functions, the preprocessor is applied in the compiler plugin's first phase (before macro inlining):

```
def foo(using CpsDirect[MyMonad]): T = body
       ↓
PhaseSelectAndGenerateShiftedMethods detects CpsDirect function
       ↓
Summon CpsPreprocessor[MyMonad]?
       ↓ Yes
Wrap: body → preprocessor.preprocess(body)
       ↓
Inlining phase expands the preprocessor macro
       ↓
PhaseCps performs CPS transformation on preprocessedBody
```

## Example: Durable Monad Preprocessor

The Durable monad needs to cache every `val` definition for replay-based execution:

```scala
// Source code
async[Durable] {
  val a = compute()
  val b = httpGet(url)
  await(sleep(1.hour))
  val c = process(b)
}

// After preprocessing (conceptual)
async[Durable] {
  val a = ctx.cached(0, 0) { compute() }
  val b = ctx.cached(0, 1) { httpGet(url) }
  await(sleep(1.hour))  // step boundary increments step index
  val c = ctx.cached(1, 0) { process(b) }
}
```

Implementation sketch:

```scala
given CpsPreprocessor[Durable] with
  inline def preprocess[A](inline body: A): A =
    ${ DurablePreprocessor.impl[A]('body) }

object DurablePreprocessor:
  def impl[A: Type](body: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*

    // Walk AST and transform:
    // 1. val x = rhs  →  val x = ctx.cached(step, idx) { rhs }
    // 2. if (cond)    →  if (ctx.cached(step, idx) { cond })
    // 3. Detect await(...) as step boundary

    // Access context via: summon[CpsMonadContext[Durable]]
    transformTree(body.asTerm).asExprOf[A]

  private def transformTree(tree: Term)(using Quotes): Term =
    // AST transformation logic
    ???
```

## Design Notes

### Context Availability

Inside the preprocessed body, `CpsMonadContext[F]` is available in scope. This allows the preprocessor to insert calls that use the context:

```scala
// Preprocessor can insert code like:
val ctx = summon[CpsMonadContext[Durable]]
ctx.cached(stepIndex, valIndex) { originalExpression }
```

### Macro vs Plugin

The preprocessor uses an `inline def` with a macro implementation. This works for both paths:

- **Macro path**: The preprocessor macro is expanded during macro expansion
- **Plugin path**: The plugin wraps the body with `preprocessor.preprocess(body)`, and the inline macro is expanded during the Inlining compiler phase (before CPS transformation in PhaseCps)

### No Preprocessor Case

If no `CpsPreprocessor[F]` is defined for a monad, the body passes through unchanged to CPS transformation. This is the default behavior for most monads.

## Related

- [CpsMonad](../MonadContexts.rst) - The base monad typeclass
- [Automatic Coloring](../AutomaticColoring.rst) - How CPS transformation works
- [Direct Style](../BasicUsage.rst) - Using `CpsDirect[F]` for direct style async code
