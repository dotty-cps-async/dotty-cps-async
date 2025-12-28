CpsPreprocessor
===============

Sometimes you need to transform code inside `async` blocks before CPS transformation occurs.
For example, you might want to log every value binding, cache computations for replay, or check that all values are immutable.

|CpsPreprocessor|_ is a typeclass that lets you hook into this stage: define a preprocessor for your monad, and all async blocks using that monad will have their code transformed before the CPS machinery runs.


Tracing Example
---------------

Let's build a preprocessor that logs every ``val`` definition inside an async block.

First, the usage - what we want to achieve:

.. code-block:: scala

   async[IO] {
     val x = computeX()
     val y = computeY(x)
     x + y
   }

   // With our tracing preprocessor, this behaves as if we wrote:
   async[IO] {
     val x = { val r = computeX(); println("x = " + r); r }
     val y = { val r = computeY(x); println("y = " + r); r }
     x + y
   }


Step 1: Define the Preprocessor Given
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The preprocessor is a given instance that delegates to a macro:

.. code-block:: scala

   import cps.*
   import scala.quoted.*

   object TracingPreprocessor:

     given [C <: CpsMonadContext[IO]]: CpsPreprocessor[IO, C] with
       transparent inline def preprocess[A](inline body: A, inline ctx: C): A =
         ${ TracingMacro.impl[A, C]('body, 'ctx) }


Step 2: Implement the Macro
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The macro receives the async block body and transforms its AST.
It must be in a separate file from the given definition (Scala 3 macro requirement).

The macro uses the low-level Reflect API to build AST nodes with correct symbol ownership:

.. code-block:: scala

   // In a separate file: TracingMacro.scala
   import scala.quoted.*

   object TracingMacro:

     def impl[A: Type, C: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
       import quotes.reflect.*

       def wrapWithTrace(name: String, rhs: Term, rhsType: TypeRepr, owner: Symbol): Term =
         // Create: { val result = rhs; println(name + " = " + result); result }
         val resultSym = Symbol.newVal(owner, "result", rhsType, Flags.EmptyFlags, Symbol.noSymbol)
         val resultValDef = ValDef(resultSym, Some(rhs.changeOwner(resultSym)))
         val resultRef = Ref(resultSym)

         // Build println call (simplified - real code would use proper method lookup)
         val printCall = '{ println(${ Expr(name) } + " = " + ${ resultRef.asExpr }) }.asTerm

         Block(List(resultValDef, printCall), resultRef)

       def transformStatement(stat: Statement, owner: Symbol): Statement =
         stat match
           case vd @ ValDef(name, tpt, Some(rhs)) =>
             val traced = wrapWithTrace(name, rhs, tpt.tpe, vd.symbol)
             ValDef.copy(vd)(name, tpt, Some(traced))
           case other => other

       def transform(term: Term, owner: Symbol): Term =
         term match
           case block @ Block(stats, expr) =>
             Block.copy(block)(stats.map(s => transformStatement(s, owner)), transform(expr, owner))
           case Inlined(call, bindings, expansion) =>
             Inlined(call, bindings, transform(expansion, owner))
           case other => other

       transform(body.asTerm, Symbol.spliceOwner).asExprOf[A]

See ``shared/src/test/scala/cps/preprocessor/TracingPreprocessorMacro.scala`` for a complete working example.


Step 3: Import and Use
^^^^^^^^^^^^^^^^^^^^^^

Import the preprocessor given, and all async blocks for that monad are automatically transformed:

.. code-block:: scala

   import TracingPreprocessor.given

   val result = async[IO] {
     val a = 1 + 1
     val b = a * 2
     b + 10
   }
   // Prints:
   //   a = 2
   //   b = 4


How It Works
------------

When you call ``async[F] { body }``:

1. The library checks if a ``CpsPreprocessor[F, C]`` is available
2. If found, it calls ``preprocessor.preprocess(body, ctx)`` before CPS transformation
3. Your macro transforms the AST as needed
4. The transformed code then goes through normal CPS transformation

This also works with direct style (``CpsDirect[F]``) and ``reify``/``reflect`` syntax.


The Context Parameter
---------------------

The preprocessor receives the monad context as ``ctx``.
This lets you generate code that uses context operations:

.. code-block:: scala

   // Your preprocessor can generate code like:
   '{ $ctx.someOperation($originalExpr) }

For example, a durable execution monad might cache values:

.. code-block:: scala

   // Generated code uses ctx.cached(...)
   val x = ctx.cached(stepIndex, valIndex) { originalComputation }


Use Cases
---------

**Tracing/Logging**: Insert logging calls around expressions (shown above).

**Durable Execution**: Cache every ``val`` so computations can be replayed after failures.
See |durable-monad|_ for a complete implementation.

**Profiling**: Wrap expressions with timing measurements.

**STM (Software Transactional Memory)**: Check that all values are immutable.

**Validation**: Insert runtime checks around specific expressions.


API Reference
-------------

See |CpsPreprocessor-api|_ in the API documentation.

If no ``CpsPreprocessor`` is defined for a monad, async blocks pass through unchanged.


.. ###########################################################################
.. ## Hyperlink definitions

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala

.. |CpsPreprocessor| replace:: ``CpsPreprocessor``
.. _CpsPreprocessor: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsPreprocessor.scala

.. |CpsMonadContext| replace:: ``CpsMonadContext``
.. _CpsMonadContext: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadContext.scala

.. |durable-monad| replace:: **durable-monad**
.. _durable-monad: https://github.com/rssh/durable-monad

.. |CpsPreprocessor-api| replace:: ``CpsPreprocessor``
.. _CpsPreprocessor-api: https://dotty-cps-async.github.io/dotty-cps-async/api/jvm/cps/CpsPreprocessor.html
