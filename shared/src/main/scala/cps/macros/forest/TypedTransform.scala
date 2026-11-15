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
    // The term is transformed with its widened type, not with its own type: it can be a union of singletons, which the
    //  transformed term does not keep (the compiler ascribes the last expression of a block with the type, which avoids
    //  the local vals: `{ val y = 3; val z = 4; (c match { case 1 => y; case _ => z }): Int }`, where the match has type
    //  `y.type | z.type`, #166)
    TransformUtil.veryWiden(t.tpe, Some(tp.tpe)).asType match
      case '[it] =>
        val r = Async.nestTransform(t.asExprOf[it], cpsCtx)
        if (!r.isAsync)
          if (!r.isChanged) CpsExpr.sync(monad, patternCode, false)
          else
            val expr = Typed(r.syncOrigin.get.asTerm, tp).asExprOf[T]
            CpsExpr.sync(monad, expr, true)
        else if (TypeRepr.of[it] =:= TypeRepr.of[T]) then r.asInstanceOf[CpsExpr[F, T]]
        else r.map('{ x => ${ Typed('x.asTerm, TypeTree.of[T]).asExprOf[T] } })
