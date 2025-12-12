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

  /**
   * Preprocessor macro that wraps val RHS with await(pure(...)).
   * This tests inserting await calls from preprocessor.
   * Uses low-level Reflect API to avoid premature type-checking of await.
   */
  def withAwaitInsertionImpl[A: Type, C <: CpsMonadContext[ComputationBound]: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*

    val awaitSymbol = Symbol.requiredMethod("cps.await")

    def wrapWithAwait(expr: Term, owner: Symbol): Term =
      val exprType = expr.tpe.widen

      // Build: ComputationBound.pure(expr)
      val cbCompanion = Ref(Symbol.requiredModule("cps.ComputationBound"))
      val pureCall = Apply(
        TypeApply(
          Select.unique(cbCompanion, "pure"),
          List(TypeTree.of(using exprType.asType))
        ),
        List(expr)
      )

      // Build: await[ComputationBound, T, ComputationBound](pureCall)(ctx, identityConversion)
      val awaitWithTypes = TypeApply(
        Ref(awaitSymbol),
        List(
          TypeTree.of[ComputationBound],
          TypeTree.of(using exprType.asType),
          TypeTree.of[ComputationBound]
        )
      )
      val awaitApply1 = Apply(awaitWithTypes, List(pureCall))

      // identityConversion[ComputationBound]
      val identityConversionRef = Ref(Symbol.requiredMethod("cps.CpsMonadConversion.identityConversion"))
      val identityConversionTyped = TypeApply(identityConversionRef, List(TypeTree.of[ComputationBound]))

      val awaitApply2 = Apply(awaitApply1, List(ctx.asTerm, identityConversionTyped))
      awaitApply2

    def transformStatement(stat: Statement, owner: Symbol): Statement =
      stat match
        case vd @ ValDef(name, tpt, Some(rhs)) =>
          val wrappedRhs = wrapWithAwait(rhs, vd.symbol)
          ValDef.copy(vd)(name, tpt, Some(wrappedRhs))
        case other =>
          other

    def transformTopLevel(term: Term, owner: Symbol): Term =
      term match
        case block @ Block(stats, expr) =>
          Block.copy(block)(
            stats.map(s => transformStatement(s, owner)),
            transformTopLevel(expr, owner)
          )
        case Inlined(call, bindings, expansion) =>
          Inlined(call, bindings, transformTopLevel(expansion, owner))
        case other =>
          other

    val transformed = transformTopLevel(body.asTerm, Symbol.spliceOwner)
    transformed.asExprOf[A]
