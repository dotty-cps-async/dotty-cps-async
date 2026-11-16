package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._

class AssignTransform[F[_]: Type, T: Type, C <: CpsMonadContext[F]: Type](cpsCtx: TransformationContext[F, T, C]):

  import cpsCtx._

  // case Assign(left,right)
  def run(using Quotes)(left: quotes.reflect.Term, right: quotes.reflect.Term): CpsExpr[F, T] =
    import quotes.reflect._
    // the declared type of the variable is expected (`ValDef(sym, None).tpt.tpe` is the declared type of `sym`). When it has
    //  singleton types of terms (`var cur: a.type`), the exact type of the right part is used: transformed terms do not keep them
    //  otherwise (#179); literal types are kept by veryWiden.
    val declaredType = ValDef(left.symbol, None).tpt.tpe
    val rightType =
      if (TransformUtil.veryWiden(declaredType, widenLiterals = false) <:< declaredType) then
        TransformUtil.veryWiden(right.tpe, Some(declaredType))
      else right.tpe
    rightType.asType match
      case '[r] =>
        val cpsRight = Async.nestTransform(right.asExprOf[r], cpsCtx)
        val optQualifier = left match
          case Select(obj, _) => Some(obj)
          // a member imported from a term path (`import O.*; n = ...`): the path is the qualifier
          case Ident(_) => QualifierPurity.importedQualifier(left)
          case _        => None
        optQualifier match
          case Some(obj) =>
            // the left part is async or changed only through the qualifier, which is transformed once.
            // widenTermRefByName: `F[h.type]` for a qualifier `h` can't be sealed
            obj.tpe.widenTermRefByName.asType match
              case '[o] =>
                val cpsObj = Async.nestTransform(obj.asExprOf[o], cpsCtx)
                if (cpsObj.isAsync) then runAsyncQualifier(left, cpsObj, right, cpsRight)
                else
                  // the qualifier can be changed (e.g. contains an unshifted lambda), but stay sync
                  val syncObj = if (cpsObj.isChanged) then cpsObj.syncOrigin.get.asTerm else obj
                  val purity = if (cpsRight.isAsync) then QualifierPurity.of(syncObj) else QualifierPurity.PurePath
                  if (purity != QualifierPurity.PurePath) then
                    // the qualifier is evaluated before the right part: `{ val q = obj; right.map(x => q.f = x) }`.
                    // The type of the field can depend on the qualifier: see QualifierPurity.bindingType
                    val qualType = QualifierPurity.bindingType(syncObj, TypeRepr.of[o])
                    val qualSym = Symbol.newVal(Symbol.spliceOwner, "xQualAssign", qualType, Flags.EmptyFlags, Symbol.noSymbol)
                    val qualValDef = ValDef(qualSym, Some(syncObj.changeOwner(qualSym)))
                    runAsyncRight(Ref(qualSym).select(left.symbol), cpsRight)
                      .prependExprs(Seq(StatementExprTreeGen(qualValDef, true)))
                  else
                    val syncLeft = if (cpsObj.isChanged) then syncObj.select(left.symbol) else left
                    runSyncLeft(syncLeft, cpsObj.isChanged, right, cpsRight)
          case None =>
            // an identifier (a local variable or a field of the enclosing class): nothing to transform
            runSyncLeft(left, false, right, cpsRight)

  private def runSyncLeft[R: Type](using
      Quotes
  )(syncLeft: quotes.reflect.Term, isLeftChanged: Boolean, right: quotes.reflect.Term, cpsRight: CpsExpr[F, R]): CpsExpr[F, T] =
    import quotes.reflect._
    if (!cpsRight.isAsync)
      if (!isLeftChanged && !cpsRight.isChanged) then CpsExpr.sync(monad, patternCode, false)
      else CpsExpr.sync(monad, Assign(syncLeft, cpsRight.syncOriginOr(right.asExprOf[R]).asTerm).asExprOf[T], true)
    else runAsyncRight(syncLeft, cpsRight)

  private def runAsyncRight[R: Type](using Quotes)(syncLeft: quotes.reflect.Term, cpsRight: CpsExpr[F, R]): CpsExpr[F, T] =
    import quotes.reflect._
    CpsExpr.async(monad, cpsRight.map[T]('{ (x: R) => ${ Assign(syncLeft, 'x.asTerm).asExprOf[T] } }).transformed)

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
