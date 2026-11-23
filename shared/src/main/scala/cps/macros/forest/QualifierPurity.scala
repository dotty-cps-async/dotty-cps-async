package cps.macros.forest

import scala.quoted._

/** Purity of a sync qualifier of an assignment `q.f = ...` or a method call `q.m(...)`, or of a sync argument `f(a, ...)`, from the
  * most to the least pure. When the right part or an argument is async, a qualifier or an argument that is not a [[PurePath]] is
  * bound to a val before it, to keep the Scala evaluation order. A monad can run the rest of the computation after the async part
  * more than once (`List`): a [[PurePath]] is evaluated in each run.
  */
private[forest] enum QualifierPurity:
  // `this`, `super`, a literal, a selection of a non-lazy val or of an object without initialization code from such a path: evaluated
  // after the async part, also more than once, it gives the same value without other effects. Also a package, the statics of a Java
  // class and a Scala.js global scope object, which are not values, and the `StringContext` of a string interpolation with such
  // parts. As in dotty, a selection from a val is a pure path also when the val can be null: its NullPointerException can be thrown
  // after the async part. (Binding it would bind selections from inline `this` proxies, `X_this.am`, whose singleton types go stale
  // when the inlined code is copied.)
  case PurePath
  // a stable path through a lazy val or an object with initialization code: its evaluation can run an initializer (as dotty's
  // `refPurity`, used by the plugin).
  case IdempotentPath
  // a method call (also a new instance, of a case class too: each evaluation gives a new one), a var, a by-name parameter, or any
  // other tree that is not a path. A qualifier changed by the transformation is classified by its new tree.
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
    // a class without initialization code (statements, fields with initializers), also in its base classes, as dotty's
    // `isNoInitsRealClass`. The namer sets `NoInits` from the body of a class when the class is completed, also for a class of the
    // current run (reading its flags completes it).
    def hasNoInits(cls: Symbol): Boolean = cls.typeRef.baseClasses.forall(_.flags.is(Flags.NoInits))
    def refPurity: QualifierPurity =
      val flags = term.symbol.flags
      if (flags.is(Flags.Mutable) || flags.is(Flags.Method) || !isByName.contains(false)) then QualifierPurity.Impure
      // an object without initialization code (`StringContext`, a synthetic companion): its evaluation has no effects. (Before
      // `Lazy`: the val of an object is lazy too.)
      else if (flags.is(Flags.Module)) then
        if (hasNoInits(term.symbol.moduleClass)) then QualifierPurity.PurePath else QualifierPurity.IdempotentPath
      else if (flags.is(Flags.Lazy)) then QualifierPurity.IdempotentPath
      else QualifierPurity.PurePath
    // `StringContext(...)`, of a string interpolation: not bound, the interpolation stays optimizable (and the qualifier of
    // a user-defined interpolator gives the same parts). Other new instances, also of case classes, are bound: a monad can run the
    // rest of the computation more than once (`List`), each run would create a new instance.
    def isStringContextApply(fun: Term): Boolean =
      fun.symbol.name == "apply" && fun.symbol.owner == Symbol.requiredModule("scala.StringContext").moduleClass
    def argPurity(arg: Term): QualifierPurity = arg match
      case Typed(Repeated(elems, _), _) => elems.map(argPurity).foldLeft(QualifierPurity.PurePath)(_.min(_))
      case _                            => of(arg)
    term match
      case This(_) | Super(_, _) | Literal(_) => QualifierPurity.PurePath
      // a spliced sequence `xs*` or an ascription
      case Typed(expr, _) => of(expr)
      case Apply(fun @ Select(qual, _), args) if isStringContextApply(fun) =>
        args.map(argPurity).foldLeft(of(qual))(_.min(_))
      case Ident(_) | Select(_, _) if isNoValue => QualifierPurity.PurePath
      // a member imported from a path (`import O.*; h`) is selected from it: the path is evaluated too
      case Ident(_)        => importedQualifier(term).map(q => refPurity.min(of(q))).getOrElse(refPurity)
      case Select(qual, _) => refPurity.min(of(qual))
      case _               => QualifierPurity.Impure

  /** The type of a val bound to a qualifier or an argument which is not a pure path: the singleton type of a stable path, which is
    * the value of `term`, is kept, because member types and the types of the next parameters can depend on it (`def g(x: A)(y:
    * x.T)`); otherwise `widened`. `term` is such a path (a lazy val, an object), or has its type (`f()`, where `def f(): b.type`), or
    * refers to a parameterless method, a by-name parameter or a var with such a declared type (`def get: b.type`).
    */
  def bindingType(using Quotes)(term: quotes.reflect.Term, widened: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect._
    // a reference can be built only to a path: `track().arr` has a TermRef type with a prefix that is not a path
    def isPathType(tpe: TypeRepr): Boolean = tpe match
      case TermRef(prefix, _) =>
        prefix match
          case NoPrefix() | ThisType(_) => true
          case _                        => isPathType(prefix)
      case _ => false
    // the declared type of a reference: `Symbol.info` is experimental, `NoPrefix.memberType` fails (see `of`), the info of a local
    // symbol is the type of a definition tree built from it
    def declaredType(tr: TermRef): TypeRepr =
      val sym = tr.termSymbol
      tr match
        case TermRef(NoPrefix(), _) =>
          if (sym.flags.is(Flags.Method)) then DefDef(sym, _ => None).returnTpt.tpe else ValDef(sym, None).tpt.tpe
        case TermRef(prefix, _) => prefix.memberType(sym)
    def stablePathType(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case tr: TermRef if isPathType(tr) =>
        if (of(Ref.term(tr)) != QualifierPurity.Impure) then Some(tr)
        else
          // a parameterless method (not a method with parameters: its reference is not a value)
          tr.widen match
            case MethodType(_, _, _) | PolyType(_, _, _) => None
            case _                                       => stablePathType(declaredType(tr).widenByName)
      case _ => None
    stablePathType(term.tpe).getOrElse(widened)

  /** The term path of a member imported from it (`import O.*; n = ...` or `import O.*; m(...)`), which is the qualifier of the
    * reference.
    */
  def importedQualifier(using Quotes)(ident: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect._
    ident.tpe match
      case TermRef(prefixRef: TermRef, _) => Some(Ref.term(prefixRef))
      case _                              => None
