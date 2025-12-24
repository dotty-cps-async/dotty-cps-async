package cps.preprocessor

import scala.quoted.*
import cps.*

/**
 * Example tracing preprocessor macro that logs every val definition.
 * This is a reference implementation for the CpsPreprocessor documentation.
 *
 * Must be in a separate file from the given definition (Scala 3 macro requirement).
 */
object TracingPreprocessorMacro:

  def impl[A: Type, C: Type](body: Expr[A], ctx: Expr[C])(using Quotes): Expr[A] =
    import quotes.reflect.*

    // Get the log method symbol
    val tracingPreprocessorModule = Symbol.requiredModule("cps.preprocessor.TracingPreprocessor")
    val logMethod = tracingPreprocessorModule.methodMember("log").head

    def wrapWithTrace(name: String, rhs: Term, rhsType: TypeRepr, owner: Symbol): Term =
      // Build: { val result = rhs; TracingPreprocessor.log(name, result); result }
      // Using low-level Reflect API to avoid ownership issues with quotes

      val resultSym = Symbol.newVal(
        owner,
        "result$trace",
        rhsType,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )

      val resultValDef = ValDef(resultSym, Some(rhs.changeOwner(resultSym)))
      val resultRef = Ref(resultSym)

      // TracingPreprocessor.log[T](name, result)
      val logCall = Apply(
        TypeApply(
          Select(Ref(tracingPreprocessorModule), logMethod),
          List(TypeTree.of(using rhsType.asType))
        ),
        List(Literal(StringConstant(name)), resultRef)
      )

      Block(List(resultValDef, logCall), resultRef)

    def transformStatement(stat: Statement, owner: Symbol): Statement =
      stat match
        case vd @ ValDef(name, tpt, Some(rhs)) =>
          val wrappedRhs = wrapWithTrace(name, rhs, tpt.tpe, vd.symbol)
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
