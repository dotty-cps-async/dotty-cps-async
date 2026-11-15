package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._

class TypedTransform[F[_]: Type, T: Type, C <: CpsMonadContext[F]: Type](cpsCtx: TransformationContext[F, T, C]):

  import cpsCtx._

  def run(using Quotes)(t: quotes.reflect.Term, tp: quotes.reflect.TypeTree): CpsExpr[F, T] =
    import quotes.reflect._
    // The term is transformed with the ascribed type, when it can: the own type of the term can be a union of singletons,
    //  which the transformed term does not keep (the compiler ascribes the last expression of a block with the type
    //  which avoids the local vals of the block: `{ val y = 3; val z = 4; (c match { case 1 => y; case _ => z }): Int }`, #166)
    val ascribed = tp.tpe
    if (t.tpe <:< ascribed && TransformUtil.veryWiden(ascribed) <:< ascribed) then
      ascribed.asType match
        case '[at] =>
          val r = Async.nestTransform(t.asExprOf[at], cpsCtx)
          if (TypeRepr.of[at] =:= TypeRepr.of[T]) && r.isAsync then r.asInstanceOf[CpsExpr[F, T]]
          else runWithInner(t, tp, r)
        case _ =>
          throw MacroError(s"Can't determinate type for ${t}", t.asExpr)
    else
      t.asExpr match
        case '{ $t1: tt1 } =>
          runWithInner(t, tp, Async.nestTransform(t1, cpsCtx))
        case _ =>
          throw MacroError(s"Can't determinate type for ${t}", t.asExpr)

  private def runWithInner[S: Type](using Quotes)(t: quotes.reflect.Term, tp: quotes.reflect.TypeTree, r: CpsExpr[F, S]): CpsExpr[F, T] =
    import quotes.reflect._
    if (!r.isAsync)
      if (!r.isChanged) CpsExpr.sync(monad, patternCode, false)
      else
        val expr = Typed(r.syncOrigin.get.asTerm, tp).asExprOf[T]
        CpsExpr.sync(monad, expr, true)
    else r.map('{ x => ${ Typed('x.asTerm, TypeTree.of[T]).asExprOf[T] } })
