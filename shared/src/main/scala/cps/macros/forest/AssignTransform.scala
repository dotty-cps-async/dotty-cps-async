package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

class AssignTransform[F[_]: Type, T: Type, C <: CpsMonadContext[F]: Type](cpsCtx: TransformationContext[F, T, C]):

  import cpsCtx._

  // case Assign(left,right)
  def run(using Quotes)(left: quotes.reflect.Term, right: quotes.reflect.Term): CpsExpr[F, T] =
    import quotes.reflect._
    // widenTermRefByName: a reference to a field or a val has a singleton type (`F[h.n.type]` can't be sealed),
    // literal types are kept (the assigned variable can have a literal type).
    right.tpe.widenTermRefByName.asType match
      case '[r] =>
        left match
          case Select(obj, _) =>
            // the left part is async or changed only through the qualifier, which is transformed once.
            obj.tpe.widenTermRefByName.asType match
              case '[o] =>
                val cpsObj = Async.nestTransform(obj.asExprOf[o], cpsCtx)
                val cpsRight = Async.nestTransform(right.asExprOf[r], cpsCtx)
                if (cpsObj.isAsync) then runAsyncQualifier(left, cpsObj, right, cpsRight)
                else
                  // the qualifier can be changed (e.g. contains an unshifted lambda), but stay sync
                  val syncLeft = if (cpsObj.isChanged) then cpsObj.syncOrigin.get.asTerm.select(left.symbol) else left
                  runSyncLeft(syncLeft, cpsObj.isChanged, right, cpsRight)
          case _ =>
            // a local variable
            runSyncLeft(left, false, right, Async.nestTransform(right.asExprOf[r], cpsCtx))

  private def runSyncLeft[R: Type](using
      Quotes
  )(syncLeft: quotes.reflect.Term, isLeftChanged: Boolean, right: quotes.reflect.Term, cpsRight: CpsExpr[F, R]): CpsExpr[F, T] =
    import quotes.reflect._
    if (!cpsRight.isAsync)
      if (!isLeftChanged && !cpsRight.isChanged) then CpsExpr.sync(monad, patternCode, false)
      else CpsExpr.sync(monad, Assign(syncLeft, cpsRight.syncOriginOr(right.asExprOf[R]).asTerm).asExprOf[T], true)
    else CpsExpr.async(monad, cpsRight.map[T]('{ (x: R) => ${ Assign(syncLeft, 'x.asTerm).asExprOf[T] } }).transformed)

  private def runAsyncQualifier[O: Type, R: Type](using
      Quotes
  )(left: quotes.reflect.Term, cpsObj: CpsExpr[F, O], right: quotes.reflect.Term, cpsRight: CpsExpr[F, R]): CpsExpr[F, T] =
    import quotes.reflect._
    if (!cpsRight.isAsync) {
      val syncRight = cpsRight.syncOriginOr(right.asExprOf[R]).asTerm
      CpsExpr.async[F, T](
        monad,
        cpsObj
          .map[T]('{ x =>
            ${ Assign('x.asTerm.select(left.symbol), syncRight).asExprOf[T] }
          })
          .transformed
      )
    } else {
      CpsExpr.async[F, T](
        monad,
        cpsObj
          .flatMap[T]('{ l =>
            ${
              cpsRight
                .map[T](
                  '{ r =>
                    ${
                      Assign('l.asTerm.select(left.symbol), 'r.asTerm).asExprOf[T]
                    }
                  }
                )
                .transformed
            }
          })
          .transformed
      )
    }
