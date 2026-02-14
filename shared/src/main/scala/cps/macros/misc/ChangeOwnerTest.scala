package cps.macros.misc

import scala.quoted._
import cps.macros.common.TransformUtil

/**
 * Minimal macros to test whether various tree manipulations break default getter Selects.
 */
object ChangeOwnerTest:

  inline def wrapInLambda[T](inline body: T): () => T =
    ${ wrapInLambdaImpl[T]('body) }

  def wrapInLambdaImpl[T: Type](body: Expr[T])(using Quotes): Expr[() => T] =
    import quotes.reflect._
    val mt = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[T])
    val lambda = Lambda(Symbol.spliceOwner, mt,
      (owner, params) => body.asTerm.changeOwner(owner))
    lambda.asExprOf[() => T]

  /**
   * Simulate the async macro: decompose block, reassemble,
   * wrap in identity function call via quasiquotes, then wrap in lambda.
   */
  inline def simulateAsyncMacro[T](inline body: T): () => T =
    ${ simulateAsyncMacroImpl[T]('body) }

  def simulateAsyncMacroImpl[T: Type](body: Expr[T])(using Quotes): Expr[() => T] =
    import quotes.reflect._

    def unwrapInlined(t: Term): Term = t match
      case Inlined(_, _, inner) => unwrapInlined(inner)
      case other => other

    val unwrapped = unwrapInlined(body.asTerm)

    unwrapped match
      case block @ Block(stats, last) =>
        // Step 1: changeOwner to spliceOwner (like Async.scala:430)
        val changed1 = block.changeOwner(Symbol.spliceOwner)
        val Block(stats1, last1) = changed1 : @unchecked
        val valDef = stats1.head.asInstanceOf[ValDef]

        // Step 2: Seal last as Expr and get sync origin
        val lastExpr = last1.asExprOf[T]

        // Step 3: ValWrappedCpsExpr.syncOrigin - reassemble block
        val lastTerm = lastExpr.asTerm.changeOwner(valDef.symbol.owner)
        val reassembled = Block(List(valDef), lastTerm)
        val blockExpr = reassembled.asExprOf[T]

        // Step 4: Wrap in identity function call via quasiquotes
        val wrappedExpr: Expr[T] = '{ identity[T]($blockExpr) }

        // Step 5: Wrap in lambda with changeOwner
        val mt = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[T])
        val lambda = Lambda(Symbol.spliceOwner, mt,
          (owner, params) => wrappedExpr.asTerm.changeOwner(owner))
        lambda.asExprOf[() => T]

      case other =>
        val mt = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[T])
        val lambda = Lambda(Symbol.spliceOwner, mt,
          (owner, params) => other.changeOwner(owner))
        lambda.asExprOf[() => T]

  /**
   * Simulate with context lambda + substituteLambdaParams (TreeMap traversal) +
   * full BlockTransform path.
   * The actual async macro:
   * 1. Wraps body in context lambda: (ctx: C) ?=> body
   * 2. Extracts the lambda, does body.changeOwner(spliceOwner).asExprOf[T]
   * 3. Runs BlockTransform which seals last separately, processes via ApplyTreeTransform
   * 4. Reassembles via SyncCpsExpr.syncOrigin / ValWrappedCpsExpr.syncOrigin
   * 5. Creates new Lambda using substituteLambdaParams + changeOwner
   */
  inline def simulateWithTreeMap[T](inline body: T): () => T =
    ${ simulateWithTreeMapImpl[T]('body) }

  def simulateWithTreeMapImpl[T: Type](body: Expr[T])(using Quotes): Expr[() => T] =
    import quotes.reflect._

    def unwrapInlined(t: Term): Term = t match
      case Inlined(_, _, inner) => unwrapInlined(inner)
      case other => other

    val unwrapped = unwrapInlined(body.asTerm)

    unwrapped match
      case block @ Block(stats, last) =>
        // Step 1: Wrap in a context lambda (simulates the (ctx: C) ?=> body input)
        val ctxType = TypeRepr.of[Int]
        val oldMt = MethodType(List("evidence"))(_ => List(ctxType), _ => TypeRepr.of[T])
        val ctxLambda = Lambda(Symbol.spliceOwner, oldMt,
          (owner, params) => block.changeOwner(owner))
        val Lambda(oldParams, oldBody) = ctxLambda : @unchecked

        // Step 2: Extract body from lambda, changeOwner to spliceOwner, seal
        // (simulates transformContextLambdaImplNoPreprocess line 430)
        val bodyChanged = oldBody.changeOwner(Symbol.spliceOwner)
        val bodyExpr = bodyChanged.asExprOf[T]

        // Step 3: BlockTransform - seal last separately, process, reassemble
        val bodyTerm = unwrapInlined(bodyExpr.asTerm)
        val Block(stats1, last1) = bodyTerm : @unchecked
        val valDef = stats1.head.asInstanceOf[ValDef]

        // seal last (BlockTransform line 54: last.asExprOf[T])
        val lastSealedExpr = last1.asExprOf[T]
        // unseal + re-seal (ApplyTreeTransform toResult safeSealAs)
        val applyTerm = unwrapInlined(lastSealedExpr.asTerm)
        val reSealedApply = applyTerm.asExprOf[T]

        // GenericSyncCpsExpr.syncOrigin:
        // Block(prev, typedLast).changeOwner(spliceOwner).asExprOf[T]
        val typedLast = {
          val lt = reSealedApply.asTerm
          if (lt.tpe =:= TypeRepr.of[T]) lt
          else Typed(lt, TypeTree.of[T])
        }
        val unitStmt = Literal(UnitConstant())
        val innerBlock = Block(List(unitStmt), typedLast)
        val innerBlockExpr = innerBlock.changeOwner(Symbol.spliceOwner).asExprOf[T]

        // ValWrappedCpsExpr.syncOrigin:
        // n.asTerm.changeOwner(valDef.symbol.owner)
        val nTerm = innerBlockExpr.asTerm.changeOwner(valDef.symbol.owner)
        val outputTerm = nTerm match
          case b @ Block(bStats, bLast) =>
            Block(valDef :: bStats, bLast)
          case other =>
            Block(List(valDef), other)
        val syncOriginExpr = outputTerm.asExprOf[T]

        // SyncCpsExpr.fLast: '{ dm.pure(${ last } : T) }
        val wrappedExpr: Expr[T] = '{ identity[T]($syncOriginExpr : T) }

        // Step 4: Create new lambda with substituteLambdaParams + changeOwner
        // (simulates transformContextLambdaImplNoPreprocess Lambda)
        val newMt = MethodType(List("evidence"))(_ => List(ctxType), _ => TypeRepr.of[T])
        val newLambda = Lambda(Symbol.spliceOwner, newMt,
          (owner, params) =>
            TransformUtil.substituteLambdaParams(oldParams, params, wrappedExpr.asTerm, owner).changeOwner(owner)
        )

        // Step 5: Apply the lambda to get T, then wrap in () => T
        val applied = Apply(Select.unique(newLambda, "apply"), List(Literal(IntConstant(0))))
        val resultExpr = applied.asExprOf[T]

        val outerMt = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[T])
        val outerLambda = Lambda(Symbol.spliceOwner, outerMt,
          (owner, params) => resultExpr.asTerm.changeOwner(owner))
        outerLambda.asExprOf[() => T]

      case other =>
        report.errorAndAbort("Expected Block")
