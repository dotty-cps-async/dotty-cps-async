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
    // (`Symbol.info` is experimental, `NoPrefix.memberType` fails: the declared type of a local parameter is the type of a ValDef
    // tree built from its symbol. Not the type of the parameter in the method type of its owner: the async block can be expanded
    // while the owner is typed, a lambda or a method with an inferred result type, and its type is not known yet.)
    def isByNameType(tpe: TypeRepr): Boolean = tpe match
      case ByNameType(_) => true
      case _             => false
    // None if the declared type is unknown
    def isByName: Option[Boolean] = term.tpe match
      case TermRef(prefix, _) =>
        prefix match
          case NoPrefix() =>
            if (term.symbol.flags.is(Flags.Param)) then Some(isByNameType(ValDef(term.symbol, None).tpt.tpe)) else Some(false)
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
    // a synthetic `apply` of a case class companion
    def isCaseClassApply(fun: Term): Boolean = fun match
      case TypeApply(f, _) => isCaseClassApply(f)
      case _ =>
        val sym = fun.symbol
        sym.name == "apply" && sym.flags.is(Flags.Synthetic) && sym.owner.flags.is(Flags.Module) &&
        sym.owner.companionClass.flags.is(Flags.Case)
    def argPurity(arg: Term): QualifierPurity = arg match
      case Typed(Repeated(elems, _), _) => elems.map(argPurity).foldLeft(QualifierPurity.PurePath)(_.min(_))
      case _                            => of(arg)
    term match
      case This(_) | Super(_, _) | Literal(_)   => QualifierPurity.PurePath
      // a new case class instance with pure arguments has no other effects (`StringContext("a", "b")` of `s"a${await(x)}b"`: not
      //  bound, the interpolation stays optimizable)
      case Apply(fun, args) if isCaseClassApply(fun) => args.map(argPurity).foldLeft(QualifierPurity.PurePath)(_.min(_))
      case Ident(_) | Select(_, _) if isNoValue => QualifierPurity.PurePath
      // a member imported from a path (`import O.*; h`) is selected from it: the path is evaluated too
      case Ident(_)                             => importedQualifier(term).map(q => refPurity.min(of(q))).getOrElse(refPurity)
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
