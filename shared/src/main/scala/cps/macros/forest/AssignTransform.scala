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
                  if (cpsRight.isAsync && !isStablePath(syncObj, allowLazy = false)) then
                    // the qualifier is evaluated before the right part: `{ val q = obj; right.map(x => q.f = x) }`.
                    // A stable path through a lazy val keeps its singleton type: the type of the field can depend on it.
                    val qualType = if (isStablePath(syncObj, allowLazy = true)) then syncObj.tpe else TypeRepr.of[o]
                    val qualSym = Symbol.newVal(Symbol.spliceOwner, "xQualAssign", qualType, Flags.EmptyFlags, Symbol.noSymbol)
                    val qualValDef = ValDef(qualSym, Some(syncObj.changeOwner(qualSym)))
                    runSyncLeft(Ref(qualSym).select(left.symbol), true, right, cpsRight)
                      .prependExprs(Seq(StatementExprTreeGen(qualValDef, true)))
                  else
                    val syncLeft = if (cpsObj.isChanged) then syncObj.select(left.symbol) else left
                    runSyncLeft(syncLeft, cpsObj.isChanged, right, cpsRight)
          case _ =>
            // an identifier (a local variable or a field of the enclosing class): nothing to transform
            runSyncLeft(left, false, right, cpsRight)

  /** `this`, `super`, or a selection of a non-lazy val or an object from a stable path: evaluating it after the right part gives the same
    * value without other effects. (A var, a lazy val, a by-name parameter or a method call is evaluated before the right part.) With
    * `allowLazy`, lazy vals are accepted too: a stable path, but its evaluation can initialize a lazy val.
    */
  private def isStablePath(using Quotes)(term: quotes.reflect.Term, allowLazy: Boolean): Boolean =
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
    def isByName: Boolean =
      val declaredType = term.tpe match
        case TermRef(prefix, _) =>
          prefix match
            case NoPrefix() => if (term.symbol.flags.is(Flags.Param)) then methodParamType(term.symbol) else None
            case _          => Some(prefix.memberType(term.symbol))
        case _ => None
      declaredType match
        case Some(ByNameType(_)) => true
        case _                   => false
    def isStableRef: Boolean =
      val flags = term.symbol.flags
      !flags.is(Flags.Mutable) && !flags.is(Flags.Method)
        && (allowLazy || !flags.is(Flags.Lazy) || flags.is(Flags.Module)) && !isByName
    term match
      case This(_) | Super(_, _) => true
      case Ident(_)              => isStableRef
      case Select(qual, _)       => isStableRef && isStablePath(qual, allowLazy)
      case _                     => false

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
