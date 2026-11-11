/** part of dotty-cps-async (C) Ruslan Shevchenko, <ruslan@shevchenko.kiev.ua>, 2020, 2021, 2022
  */
package cps.macros.misc

import scala.quoted.*

import cps.macros.common.*
import cps.macros.forest.*

object WithOptExprProxy:

  def apply[T: Type, S: Type](name: String, originExpr: Expr[T])(buildExpr: Expr[T] => Expr[S])(using Quotes): Expr[S] =
    import quotes.reflect.*
    val DEBUG = false
    if (DEBUG) {
      TransformUtil.dummyMapper(originExpr.asTerm, Symbol.spliceOwner)
    }
    originExpr.asTerm match
      case Ident(_)   => buildExpr(originExpr)
      case originTerm =>
        // Synthetic: proxy can be unused (when the transformed body does not call the monad), which should not
        // be reported by -Wunused:locals (https://github.com/dotty-cps-async/dotty-cps-async/issues/143)
        val proxy = Symbol.newVal(Symbol.spliceOwner, name, originExpr.asTerm.tpe.widen, Flags.Synthetic, Symbol.noSymbol)
        val proxyValDef = ValDef(proxy, Some(originTerm))
        val proxyIdent = Ref(proxy).asExprOf[T]
        val buildTerm = buildExpr(proxyIdent).asTerm.changeOwner(Symbol.spliceOwner)
        val resTerm = buildTerm match
          case lambdaExpr @ Lambda(params, body) => Block(List(proxyValDef), lambdaExpr)
          case blockExpr @ Block(stats, expr) =>
            Block(proxyValDef :: stats, expr)
          case expr => Block(List(proxyValDef), expr)
        resTerm.asExprOf[S]
