package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._

object IfTransform:

  /** ''' '{ if ($cond) $ifTrue else $ifFalse } '''
    */
  def run[F[_]: Type, T: Type, C <: CpsMonadContext[F]: Type](
      cpsCtx: TransformationContext[F, T, C],
      cond: Expr[Boolean],
      ifTrue: Expr[T],
      ifFalse: Expr[T]
  )(using Quotes): CpsExpr[F, T] =
    import quotes.reflect._
    import util._
    import cpsCtx._
    val cR = Async.nestTransform(cond, cpsCtx)
    val tR = Async.nestTransform(ifTrue, cpsCtx)
    val fR = Async.nestTransform(ifFalse, cpsCtx)
    var isAsync = true

    // a sync part can be changed (e.g. contains an unshifted lambda): then the changed tree should be used
    def syncPart[A](r: CpsExpr[F, A], origin: Expr[A]): Expr[A] =
      if (r.isChanged) r.syncOrigin.get else origin

    val cnBuild = {
      if (!cR.isAsync) then
        if (!tR.isAsync && !fR.isAsync) then
          isAsync = false
          if (cR.isChanged || tR.isChanged || fR.isChanged) then
            CpsExpr.sync(
              monad,
              '{
                if (${ cR.syncOrigin.get })
                  ${ tR.syncOrigin.get }
                else
                  ${ fR.syncOrigin.get }
              },
              true
            )
          else CpsExpr.sync(monad, patternCode, false)
        else
          CpsExpr.async[F, T](
            monad,
            '{
              if (${ syncPart(cR, cond) })
                ${ tR.transformed }
              else
                ${ fR.transformed }
            }
          )
      else // (cR.isAsync)
        def condAsyncExpr() = cR.transformed
        if (!tR.isAsync && !fR.isAsync)
          CpsExpr.async[F, T](
            monad,
            '{
              ${ monad }.map(
                ${ condAsyncExpr() }
              )(c =>
                if (c) {
                  ${ syncPart(tR, ifTrue) }
                } else {
                  ${ syncPart(fR, ifFalse) }
                }
              )
            }
          )
        else
          CpsExpr.async[F, T](
            monad,
            '{
              ${ monad }.flatMap(
                ${ condAsyncExpr() }
              )(c =>
                if (c) {
                  ${ tR.transformed }
                } else {
                  ${ fR.transformed }
                }
              )
            }
          )
    }
    cnBuild
