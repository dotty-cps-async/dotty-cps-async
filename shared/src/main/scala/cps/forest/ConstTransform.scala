package cps.forest

import scala.quoted._
import scala.compiletime._

import cps._


object ConstTransform:

  // we know, that f is match to Const
  //(see rootTransform)
  def run[F[_]:Type,T:Type](using Quotes)(cpsCtx: TransformationContext[F,T],
                                          constTerm: quotes.reflect.Literal):CpsExpr[F,T] =
     import quotes.reflect._
     if (cpsCtx.flags.debugLevel >= 10) then
        cpsCtx.log(s"const: T=${Type.show[T]}, code=${cpsCtx.patternCode.show}")
     //CpsExpr.sync(cpsCtx.monad, Typed(constTerm, TypeTree.of[T] ).asExprOf[T], true) 
     // policy: where to insert typed[?]
     CpsExpr.sync(cpsCtx.monad, constTerm.asExprOf[T], false) 


