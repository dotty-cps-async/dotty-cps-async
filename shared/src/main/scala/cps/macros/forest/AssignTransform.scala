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
    // widen: the type of the assigned field is a singleton type (`F[h.n.type]` can't be sealed)
    left.tpe.widen.asType match
      case '[lt] =>
        val cpsLeft = Async.nestTransform(left.asExprOf[lt], cpsCtx)
        // shpuld have to structure in such waym as workarround against
        //
        runWithLeft(left, right, cpsLeft)

  def runWithLeft[L: Type](using
      Quotes
  )(left: quotes.reflect.Term, right: quotes.reflect.Term, cpsLeft: CpsExpr[F, L]): CpsExpr[F, T] = {
    import quotes.reflect._
    right.tpe.widen.asType match
      case '[rt] =>
        val cpsRight = Async.nestTransform(right.asExprOf[rt], cpsCtx)
        run1(left, right, cpsLeft, cpsRight)
  }

  // a sync part can be changed (e.g. contains an unshifted lambda): then the changed tree should be used
  private def syncPart[A](using Quotes)(r: CpsExpr[F, A], origin: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect._
    if (r.isChanged) r.syncOrigin.get.asTerm else origin

  def run1[L: Type, R: Type](using
      Quotes
  )(left: quotes.reflect.Term, right: quotes.reflect.Term, cpsLeft: CpsExpr[F, L], cpsRight: CpsExpr[F, R]): CpsExpr[F, T] =
    import quotes.reflect._
    if (!cpsLeft.isAsync) {
      // the left part can be changed (in the qualifier of select), but stay sync
      val syncLeft = if (cpsLeft.isChanged) changedSyncLeft(left) else left
      if (!cpsRight.isAsync)
        if (!cpsLeft.isChanged && !cpsRight.isChanged) then CpsExpr.sync(monad, patternCode, false)
        else CpsExpr.sync(monad, Assign(syncLeft, syncPart(cpsRight, right)).asExprOf[T], true)
      else // !cpsLeft.isAsync && cpsRight.isAsync
        CpsExpr.async(monad, cpsRight.map[T]('{ (x: R) => ${ Assign(syncLeft, 'x.asTerm).asExprOf[T] } }).transformed)
    } else { // (cpsLeft.isAsync) {
      left match
        case Select(obj, sym) =>
          obj.tpe.widen.asType match
            case '[ot] =>
              val lu = Async.nestTransform(obj.asExprOf[ot], cpsCtx)
              run2(left, right, cpsLeft, cpsRight, lu)
        case _ => // non-assignable entity ?
          throw MacroError("assign to async non-select is impossible", patternCode)
    }

  /** left is changed, but sync: the qualifier of the select is changed.
    */
  private def changedSyncLeft(using Quotes)(left: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect._
    left match
      case Select(obj, _) =>
        obj.tpe.widen.asType match
          case '[ot] =>
            val cpsObj = Async.nestTransform(obj.asExprOf[ot], cpsCtx)
            if (cpsObj.isChanged) then cpsObj.syncOrigin.get.asTerm.select(left.symbol) else left
      case _ => left

  def run2[L: Type, R: Type, LU: Type](using
      Quotes
  )(
      left: quotes.reflect.Term,
      right: quotes.reflect.Term,
      cpsLeft: CpsExpr[F, L],
      cpsRight: CpsExpr[F, R],
      cpsLu: CpsExpr[F, LU]
  ): CpsExpr[F, T] =
    import quotes.reflect._
    if (!cpsRight.isAsync) {
      val syncRight = syncPart(cpsRight, right)
      CpsExpr.async[F, T](
        monad,
        cpsLu
          .map[T]('{ x =>
            ${ Assign('x.asTerm.select(left.symbol), syncRight).asExprOf[T] }
          })
          .transformed
      )
    } else {
      CpsExpr.async[F, T](
        monad,
        cpsLu
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
