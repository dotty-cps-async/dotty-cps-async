# CpsPreprocessor - Expression Preprocessing for Async Blocks

## Overview

`CpsPreprocessor[F[_], C <: CpsMonadContext[F]]` is a typeclass that allows monads to transform expressions inside `async` blocks before CPS (Continuation-Passing Style) transformation occurs. This enables monad-specific AST transformations such as:

- **Durable monad**: Wrap all `val` definitions with caching calls for replay-based execution
- **Tracing monad**: Insert trace/logging calls around expressions
- **STM monad**: Add savepoint markers for transactional memory

## API

```scala
package cps

trait CpsPreprocessor[F[_], C <: CpsMonadContext[F]]:
  /**
   * Transform the body of an async block before CPS transformation.
   *
   * @param body The body expression inside the async block
   * @param ctx The monad context, passed explicitly for use in generated code
   * @tparam A The result type of the body expression
   * @return The transformed body expression
   */
  inline def preprocess[A](inline body: A, inline ctx: C): A
```

## Usage

### Defining a Preprocessor

A preprocessor is typically implemented as a macro that transforms the AST. The context is passed explicitly so the macro can directly reference it in generated code:

```scala
import cps.*
import scala.quoted.*

given CpsPreprocessor[MyMonad, MyContext] with
  inline def preprocess[A](inline body: A, inline ctx: MyContext): A =
    ${ MyPreprocessorMacro.impl[A, MyContext]('body, 'ctx) }

object MyPreprocessorMacro:
  def impl[A: Type, C <: CpsMonadContext[MyMonad]: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*
    // Transform the AST using $ctx directly
    '{ $ctx.someOperation($body) }
```

### How It Works

The preprocessor is applied automatically in two transformation paths:

#### 1. Macro Path (`async[F] { }`)

When using `async[F] { body }`, the preprocessor is applied inside `transformContextLambdaImpl`:

```
async[MyMonad] { body }
       ↓
extractLambda gets (params, body) where params contains ctx
       ↓
Summon CpsPreprocessor[MyMonad, C]?
       ↓ Yes
preprocessor.preprocess(body)(ctx) → preprocessedBody
       ↓
CPS transformation on preprocessedBody
```

#### 2. Direct Style Path (`def foo(using CpsDirect[F])`)

For direct style functions, the preprocessor is applied in the compiler plugin's first phase (before macro inlining):

```
def foo(using ctx: CpsDirect[MyMonad]): T = body
       ↓
PhaseSelectAndGenerateShiftedMethods detects CpsDirect function
       ↓
Summon CpsPreprocessor[MyMonad, CpsDirect[MyMonad]]?
       ↓ Yes
Wrap: body → preprocessor.preprocess(body)(ctx)
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
given CpsPreprocessor[Durable, DurableContext] with
  inline def preprocess[A](inline body: A, inline ctx: DurableContext): A =
    ${ DurablePreprocessor.impl[A]('body, 'ctx) }

object DurablePreprocessor:
  def impl[A: Type](body: Expr[A], ctx: Expr[DurableContext])(using Quotes): Expr[A] =
    import quotes.reflect.*

    // Walk AST and transform:
    // 1. val x = rhs  →  val x = $ctx.cached(step, idx) { rhs }
    // 2. if (cond)    →  if ($ctx.cached(step, idx) { cond })
    // 3. Detect await(...) as step boundary

    transformTree(body.asTerm, ctx).asExprOf[A]

  private def transformTree(tree: Term, ctx: Expr[DurableContext])(using Quotes): Term =
    // AST transformation logic
    ???
```

## Design Notes

### Explicit Context Parameter

The monad context is passed explicitly to `preprocess` as the `ctx` parameter. This allows the preprocessor macro to directly reference the context in generated code:

```scala
// Preprocessor can generate code like:
'{ $ctx.cached(stepIndex, valIndex) { $originalExpression } }
```

### Type Parameters

The preprocessor has two type parameters:
- `F[_]` - The monad type (e.g., `Future`, `IO`, `Durable`)
- `C <: CpsMonadContext[F]` - The context type (e.g., `CpsDirect[Future]`, `DurableContext`)

This allows different preprocessors for different context types of the same monad, enabling context-specific transformations. For example, a `DurableContext` might have a `cached` method that a generic `CpsMonadContext[Durable]` doesn't have.

### Macro vs Plugin

The preprocessor uses an `inline def` with a macro implementation. This works for both paths:

- **Macro path**: The preprocessor macro is expanded during macro expansion
- **Plugin path**: The plugin wraps the body with `preprocessor.preprocess(body)(ctx)`, and the inline macro is expanded during the Inlining compiler phase (before CPS transformation in PhaseCps)

### No Preprocessor Case

If no `CpsPreprocessor[F, C]` is defined for a monad and context type, the body passes through unchanged to CPS transformation. This is the default behavior for most monads.

## Related

- [CpsMonad](../MonadContexts.rst) - The base monad typeclass
- [Automatic Coloring](../AutomaticColoring.rst) - How CPS transformation works
- [Direct Style](../BasicUsage.rst) - Using `CpsDirect[F]` for direct style async code
