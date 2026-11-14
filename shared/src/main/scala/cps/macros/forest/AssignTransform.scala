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
    // the exact type of the right part: the assigned variable can have a literal or a singleton type
    right.tpe.asType match
      case '[r] =>
        val cpsRight = Async.nestTransform(right.asExprOf[r], cpsCtx)
        left match
          case Select(obj, _) =>
            // the left part is async or changed only through the qualifier, which is transformed once.
            // widenTermRefByName: `F[h.type]` for a qualifier `h` can't be sealed
            obj.tpe.widenTermRefByName.asType match
              case '[o] =>
                val cpsObj = Async.nestTransform(obj.asExprOf[o], cpsCtx)
                if (cpsObj.isAsync) then runAsyncQualifier(left, cpsObj, right, cpsRight)
                else
                  // the qualifier can be changed (e.g. contains an unshifted lambda), but stay sync
                  val syncObj = if (cpsObj.isChanged) then cpsObj.syncOrigin.get.asTerm else obj
                  val purity = if (cpsRight.isAsync) then qualifierPurity(syncObj) else QualifierPurity.PurePath
                  if (purity != QualifierPurity.PurePath) then
                    // the qualifier is evaluated before the right part: `{ val q = obj; right.map(x => q.f = x) }`.
                    // A path through a lazy val or an object keeps its singleton type: the type of the field can depend on it.
                    val qualType = if (purity == QualifierPurity.IdempotentPath) then syncObj.tpe else TypeRepr.of[o]
                    val qualSym = Symbol.newVal(Symbol.spliceOwner, "xQualAssign", qualType, Flags.EmptyFlags, Symbol.noSymbol)
                    val qualValDef = ValDef(qualSym, Some(syncObj.changeOwner(qualSym)))
                    runAsyncRight(Ref(qualSym).select(left.symbol), cpsRight)
                      .prependExprs(Seq(StatementExprTreeGen(qualValDef, true)))
                  else
                    val syncLeft = if (cpsObj.isChanged) then syncObj.select(left.symbol) else left
                    runSyncLeft(syncLeft, cpsObj.isChanged, right, cpsRight)
          case _ =>
            // an identifier (a local variable or a field of the enclosing class): nothing to transform
            runSyncLeft(left, false, right, cpsRight)

  /** Whether a sync qualifier of the left part can be evaluated after the right part, as dotty's `TreeInfo.exprPurity` for paths. */
  private def qualifierPurity(using Quotes)(term: quotes.reflect.Term): QualifierPurity =
    import quotes.reflect._
    // the reference itself has a TermRef type; the declared type of a by-name parameter is a by-name type.
    // (`Symbol.info` is experimental, `NoPrefix.memberType` fails: a method parameter's type is taken from the method type,
    // as in MethodParamsDescriptor.isByName)
    def methodParamType(sym: Symbol): Option[TypeRepr] =
      def find(tpe: TypeRepr): Option[TypeRepr] = tpe match
        case mt: MethodType =>
          mt.paramNames.indexOf(sym.name) match
            case -1 => find(mt.resType)
            case i  => Some(mt.paramTypes(i))
        case pt: PolyType => find(pt.resType)
        case _            => None
      find(sym.owner.termRef.widen)
    def isByNameType(tpe: TypeRepr): Boolean = tpe match
      case ByNameType(_) => true
      case _             => false
    // None if the declared type is unknown
    def isByName: Option[Boolean] = term.tpe match
      case TermRef(prefix, _) =>
        prefix match
          case NoPrefix() =>
            if (term.symbol.flags.is(Flags.Param)) then methodParamType(term.symbol).map(isByNameType) else Some(false)
          case _ => Some(isByNameType(prefix.memberType(term.symbol)))
      case _ => None
    def refPurity: QualifierPurity =
      val flags = term.symbol.flags
      if (flags.is(Flags.Package)) then QualifierPurity.PurePath
      else if (flags.is(Flags.Mutable) || flags.is(Flags.Method) || !isByName.contains(false)) then QualifierPurity.Impure
      else if (flags.is(Flags.Lazy) || flags.is(Flags.Module)) then QualifierPurity.IdempotentPath
      else QualifierPurity.PurePath
    term match
      case This(_) | Super(_, _) => QualifierPurity.PurePath
      case Ident(_)              => refPurity
      case Select(qual, _)       => refPurity.min(qualifierPurity(qual))
      case _                     => QualifierPurity.Impure

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

/** Purity of a sync qualifier of an assignment, from the most to the least pure. */
private[forest] enum QualifierPurity:
  // `this`, `super`, a selection of a non-lazy val or a package from such a path: evaluated after the right part, it gives the same
  // value without other effects.
  case PurePath
  // a stable path through a lazy val or an object: its evaluation can run an initializer.
  case IdempotentPath
  // a method call, a var, a by-name parameter, a qualifier changed by the transformation, ...
  case Impure

  def min(other: QualifierPurity): QualifierPurity =
    if (ordinal >= other.ordinal) then this else other
