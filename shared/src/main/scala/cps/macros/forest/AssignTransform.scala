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
        val optQualifier = left match
          case Select(obj, _) => Some(obj)
          case Ident(_) =>
            left.tpe match
              case TermRef(prefixRef: TermRef, _) =>
                // a member imported from a term path (`import O.*; n = ...`): the path is the qualifier
                Some(Ref.term(prefixRef))
              case _ => None
          case _ => None
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
                  val purity = if (cpsRight.isAsync) then qualifierPurity(syncObj) else QualifierPurity.PurePath
                  if (purity != QualifierPurity.PurePath) then
                    // the qualifier is evaluated before the right part: `{ val q = obj; right.map(x => q.f = x) }`.
                    // A singleton type of a stable path (a lazy val or an object, or the result type of `def f(): b.type`) is kept:
                    // the type of the field can depend on it. (A `var` with a singleton type is widened: its declared type is not known)
                    val qualType = syncObj.tpe match
                      case tr: TermRef if qualifierPurity(Ref.term(tr)) != QualifierPurity.Impure => tr
                      case _                                                                         => TypeRepr.of[o]
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
    // the statics of a Java class and a Scala.js global scope object are not values: never bound, no initialization order
    def isNoValueModule: Boolean =
      term.symbol.flags.is(Flags.Module) && (term.symbol.flags.is(Flags.JavaDefined) ||
        term.symbol.moduleClass.annotations.exists(_.tpe.typeSymbol.fullName == "scala.scalajs.js.annotation.JSGlobalScope"))
    def refPurity: QualifierPurity =
      val flags = term.symbol.flags
      if (flags.is(Flags.Package) || isNoValueModule) then QualifierPurity.PurePath
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
  // `this`, `super`, a selection of a non-lazy val, a package or a Java statics object from such a path: evaluated after the right part,
  // it gives the same value without other effects.
  case PurePath
  // a stable path through a lazy val or an object: its evaluation can run an initializer. Unlike dotty's `refPurity` (used by the
  // plugin), every Scala object counts here: whether it has initializers is not checked.
  case IdempotentPath
  // a method call, a var, a by-name parameter, or any other tree that is not a path. A qualifier changed by the transformation is
  // classified by its new tree.
  case Impure

  def min(other: QualifierPurity): QualifierPurity =
    if (ordinal >= other.ordinal) then this else other
