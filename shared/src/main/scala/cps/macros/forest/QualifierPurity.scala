package cps.macros.forest

import scala.quoted._

/** Purity of a sync qualifier of an assignment `q.f = ...` or a method call `q.m(...)`, from the most to the least pure. When the
  * right part or an argument is async, a qualifier that is not a [[PurePath]] is bound to a val before it, to keep the Scala
  * evaluation order.
  */
private[forest] enum QualifierPurity:
  // `this`, `super`, a literal, a selection of a non-lazy val from such a path: evaluated after the async part, it gives the same value
  // without other effects. Also a package, the statics of a Java class and a Scala.js global scope object, which are not values.
  case PurePath
  // a stable path through a lazy val or an object: its evaluation can run an initializer. Unlike dotty's `refPurity` (used by the
  // plugin), every Scala object counts here: whether it has initializers is not checked.
  case IdempotentPath
  // a method call, a var, a by-name parameter, or any other tree that is not a path. A qualifier changed by the transformation is
  // classified by its new tree.
  case Impure

  def min(other: QualifierPurity): QualifierPurity =
    if (ordinal >= other.ordinal) then this else other

private[forest] object QualifierPurity:

  /** The purity of a sync qualifier, as dotty's `TreeInfo.exprPurity` for paths. */
  def of(using Quotes)(term: quotes.reflect.Term): QualifierPurity =
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
    // a package, the statics of a Java class and a Scala.js global scope object are not values: never bound, whatever the prefix is
    // (`js.Dynamic.global` is in the object `js.Dynamic`), no initialization order
    def isNoValue: Boolean =
      term.symbol.flags.is(Flags.Package) || term.symbol.flags.is(Flags.Module) && (term.symbol.flags.is(Flags.JavaDefined) ||
        term.symbol.moduleClass.annotations.exists(_.tpe.typeSymbol.fullName == "scala.scalajs.js.annotation.JSGlobalScope"))
    def refPurity: QualifierPurity =
      val flags = term.symbol.flags
      if (flags.is(Flags.Mutable) || flags.is(Flags.Method) || !isByName.contains(false)) then QualifierPurity.Impure
      else if (flags.is(Flags.Lazy) || flags.is(Flags.Module)) then QualifierPurity.IdempotentPath
      else QualifierPurity.PurePath
    term match
      case This(_) | Super(_, _) | Literal(_)   => QualifierPurity.PurePath
      case Ident(_) | Select(_, _) if isNoValue => QualifierPurity.PurePath
      case Ident(_)                             => refPurity
      case Select(qual, _)                      => refPurity.min(of(qual))
      case _                                    => QualifierPurity.Impure

  /** The type of a val bound to a qualifier which is not a pure path: the singleton type of a stable path (a lazy val or an object,
    * or the result type of `def f(): b.type`) is kept, because member types can depend on it; otherwise `widened`. (A `var` with a
    * singleton type is widened: its declared type is not known.)
    */
  def bindingType(using Quotes)(qualifier: quotes.reflect.Term, widened: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect._
    // a reference can be built only to a path: `track().arr` has a TermRef type with a prefix that is not a path
    def isPathType(tpe: TypeRepr): Boolean = tpe match
      case TermRef(prefix, _) =>
        prefix match
          case NoPrefix() | ThisType(_) => true
          case _                        => isPathType(prefix)
      case _ => false
    qualifier.tpe match
      case tr: TermRef if isPathType(tr) && of(Ref.term(tr)) != QualifierPurity.Impure => tr
      case _                                                                           => widened

  /** The term path of a member imported from it (`import O.*; n = ...` or `import O.*; m(...)`), which is the qualifier of the
    * reference.
    */
  def importedQualifier(using Quotes)(ident: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect._
    ident.tpe match
      case TermRef(prefixRef: TermRef, _) => Some(Ref.term(prefixRef))
      case _                              => None
