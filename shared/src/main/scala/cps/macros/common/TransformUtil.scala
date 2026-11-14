package cps.macros.common

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

object TransformUtil:

  def createFunctionType(using
      Quotes
  )(argTypes: List[quotes.reflect.TypeRepr], resultType: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect._
    val funSymbol = defn.FunctionClass(argTypes.size)
    val funTypeTree: TypeTree = TypeIdent(funSymbol)
    funTypeTree.tpe.appliedTo(argTypes.map(_.widen) :+ resultType.widen)

  def substituteLambdaParams(using
      qctx: Quotes
  )(
      oldParams: List[quotes.reflect.ValDef],
      newParams: List[quotes.reflect.Tree],
      body: quotes.reflect.Term,
      owner: quotes.reflect.Symbol
  ): quotes.reflect.Term =
    import quotes.reflect._
    val paramsMap = oldParams.zipWithIndex.map { case (tree, index) => (tree.symbol, index) }.toMap
    val indexedArgs = newParams.toIndexedSeq

    // def lookupParamTerm(sym: Symbol): Option[Term] =
    //      paramsMap.get(sym).map(i => Ref(indexedArgs(i).symbol))
    val assoc: Map[Symbol, Tree] = paramsMap.map((k, i) => (k, Ref(indexedArgs(i).symbol)))
    changeSymsInTerm(assoc, body, owner)

  /** Build a lambda with one parameter as context function `(p: A) ?=> R` and cast it to `A => R` (both are erased to
    * `Function1`).
    *
    * Used when we rebuild the context function `C ?=> T` of async block as `C => F[T]`: the new parameter receives a simple
    * name instead of the compiler-generated `contextual$N`, so as an explicit parameter it would be reported by
    * `-Wunused:explicits` when the transformed body does not use the monad context
    * (https://github.com/dotty-cps-async/dotty-cps-async/issues/143). The given parameter of a synthetic lambda is not reported.
    */
  def contextLambdaAsFunction(using
      Quotes
  )(
      owner: quotes.reflect.Symbol,
      paramName: String,
      paramType: quotes.reflect.TypeRepr,
      resultType: quotes.reflect.TypeRepr
  )(rhsFn: (quotes.reflect.Symbol, List[quotes.reflect.Tree]) => quotes.reflect.Tree): quotes.reflect.Term =
    import quotes.reflect._
    val mt = MethodType(MethodTypeKind.Contextual)(List(paramName))(_ => List(paramType), _ => resultType)
    val lambda = Lambda(owner, mt, rhsFn)
    TypeApply(Select.unique(lambda, "asInstanceOf"), List(Inferred(createFunctionType(List(paramType), resultType))))

  /** Annotations of the scrutinee type which disable exhaustivity checking (`@unchecked` and `@RuntimeChecked`, which is
    * added by `.runtimeChecked`).
    */
  def exhaustivityUncheckedAnnotations(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.Term] =
    import quotes.reflect._
    tpe match
      case AnnotatedType(underlying, annot) =>
        val rest = exhaustivityUncheckedAnnotations(underlying)
        annot.tpe.typeSymbol.fullName match
          case "scala.unchecked" | "scala.annotation.internal.RuntimeChecked" => annot :: rest
          case _                                                            => rest
      case _ => Nil

  /** New selector `x` of a match with the annotations from [[exhaustivityUncheckedAnnotations]] of the original selector: they
    * are lost in the type of a new lambda parameter (as typer does for irrefutable patterns or partial functions).
    */
  def withUncheckedAnnotations(using
      Quotes
  )(x: quotes.reflect.Term, annotations: List[quotes.reflect.Term]): quotes.reflect.Term =
    import quotes.reflect._
    if (annotations.isEmpty) then x
    else Typed(x, Inferred(annotations.foldRight(x.tpe.widen)((annot, tp) => AnnotatedType(tp, annot))))

  /** Substitute the symbols from `association` in `body` (references to lambda parameters by references to the parameters of a
    * new lambda), also in the types of all trees. Definitions at the top level of the result are owned by `owner`.
    *
    * The substitution is the beta-reduction of `((s1, ..., sn) => body)(a1, ..., an)` for `association` `si -> ai`, done by
    * `Term.betaReduce`: the compiler substitutes parameters by the arguments which are paths with TreeTypeMap. It changes the
    * types of all trees, including patterns, and gives new symbols to the local definitions whose types refer to the
    * substituted symbols. For example, `val c = n.children` with type `(n.Child, n.Child)` becomes a new val of type
    * `(n1.Child, n1.Child)` when `n` is substituted by `n1`, so `c._1` has type `n1.Child`
    * (https://github.com/dotty-cps-async/dotty-cps-async/issues/150). A TreeMap can't do this: the quotes API has no types of
    * pattern trees (`Unapply`, `Wildcard`, `Bind`), which are pickled.
    *
    * The lambda is built only for the reduction: its parameter trees have the symbols from `association` and its body is `body`
    * (which may have another type than the lambda the symbols come from); its result type is `Any`, so the reduction does not
    * insert a cast of the result.
    */
  def changeSymsInTerm(using
      Quotes
  )(
      association: Map[quotes.reflect.Symbol, quotes.reflect.Tree],
      body: quotes.reflect.Term,
      owner: quotes.reflect.Symbol
  ): quotes.reflect.Term = {
    import quotes.reflect._
    if (association.isEmpty) then body
    else
      val (syms, values) = association.toList.unzip
      val args = values.map {
        case arg: Term => arg
        case other =>
          throw MacroError(s"term expected for substitution of a symbol, we have ${other}", body.asExpr)
      }
      // ValDef(sym, None) has the declared type of sym as tpt
      val params = syms.map(sym => ValDef(sym, None))
      val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => TypeRepr.of[Any])
      val lambda = Lambda(owner, mt, (_, _) => Literal(NullConstant())) match
        case block @ Block(List(ddef: DefDef), closure) =>
          // the result of the reduction receives the position of the application, so we wrap the body to keep its position
          val wrappedBody = Block(Nil, body.changeOwner(ddef.symbol))
          val nDdef = DefDef.copy(ddef)(ddef.name, List(TermParamClause(params)), ddef.returnTpt, Some(wrappedBody))
          Block.copy(block)(List(nDdef), closure)
        case other =>
          throw MacroError(s"Internal error: Lambda should be a block with a closure, we have $other", body.asExpr)
      Term.betaReduce(Apply(Select.unique(lambda, "apply"), args)) match
        case Some(reduced) =>
          val unwrapped = reduced match
            case Block(Nil, expr) => expr
            case other            => other
          // the definitions are owned by the owner of the macro expansion after the reduction
          unwrapped.changeOwner(owner)
        case None =>
          throw MacroError(s"Internal error: can't substitute ${syms.mkString(",")} in the body", body.asExpr)
  }

  /** The closure type of a lambda `Block(List(DefDef), Closure(meth, tpt))`, defined for a SAM type (also for a partial
    * function), with the result type of its method. A rebuilt lambda should keep them: `Lambda(...)` builds a function closure,
    * and for a partial function ExpandSAMs builds `isDefinedAt` and `applyOrElse` from the closure type and the cases.
    */
  def lambdaClosureType(using Quotes)(lambda: quotes.reflect.Term): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =
    import quotes.reflect._
    lambda match
      case Block(List(ddef: DefDef), Closure(_, Some(tpe))) if !tpe.isFunctionType && !tpe.isContextFunctionType =>
        Some((tpe, ddef.returnTpt.tpe))
      case Inlined(_, Nil, inner) => lambdaClosureType(inner)
      case _                      => None

  /** Set the closure type of a lambda, built by `Lambda(...)` as `Block(List(DefDef), Closure)`. */
  def withClosureType(using Quotes)(lambda: quotes.reflect.Term, tpe: quotes.reflect.TypeRepr): quotes.reflect.Term =
    import quotes.reflect._
    lambda match
      case Block(List(ddef: DefDef), closure @ Closure(meth, _)) =>
        Block.copy(lambda)(List(ddef), Closure.copy(closure)(meth, Some(tpe)))
      case other =>
        throw MacroError(s"Internal error: Lambda should be a block with a closure, we have $other", lambda.asExpr)

  /** widen, which works over 'or' and 'and' types. (bug in dotty?)
    */
  def veryWiden(using qctx: Quotes)(tp: qctx.reflect.TypeRepr): qctx.reflect.TypeRepr =
    import quotes.reflect._
    tp match
      case OrType(lhs, rhs) =>
        val nLhs = veryWiden(lhs)
        val nRhs = veryWiden(rhs)
        if (nLhs =:= nRhs) then nLhs
        else OrType(veryWiden(lhs), veryWiden(rhs))
      case other => tp.widen

  def ensureTyped(using qctx: Quotes)(term: qctx.reflect.Term, tp: qctx.reflect.TypeRepr): qctx.reflect.Term =
    import quotes.reflect._
    if (term.tpe =:= tp) then term
    else Typed(term, Inferred(tp))

  def safeShow(using Quotes)(t: quotes.reflect.Tree): String =
    try t.show
    catch
      case ex: Exception =>
        t.toString

  def findAllOwnersIn(using Quotes)(tree: quotes.reflect.Tree): Map[Int, quotes.reflect.Tree] =
    import quotes.reflect._
    import util._
    val search = new TreeAccumulator[Map[Int, Tree]] {

      def foldTree(x: Map[Int, Tree], tree: Tree)(owner: Symbol): Map[Int, Tree] =
        foldOverTree(x, tree)(owner)

      override def foldOverTree(x: Map[Int, Tree], tree: Tree)(owner: Symbol): Map[Int, Tree] = {
        tree match
          case t: Definition =>
            val maybeOwner = t.symbol.maybeOwner
            if (maybeOwner != Symbol.noSymbol && maybeOwner != owner) {
              x.get(maybeOwner.hashCode) match
                case Some(other) => x
                case None        => x.updated(maybeOwner.hashCode, t)
            } else
              x
          case TypedOrTest(expr, tpt) =>
            // workarround against https://github.com/lampepfl/dotty/issues/14393
            foldTree(foldOverTree(x, expr)(owner), tpt)(owner)
          case _ =>
            super.foldOverTree(x, tree)(owner)
      }
    }
    search.foldTree(Map.empty, tree)(Symbol.spliceOwner)

  def findDefinitionWithoutSymbol(using Quotes)(tree: quotes.reflect.Tree): Option[quotes.reflect.Tree] =
    import quotes.reflect._
    import util._
    val search = new TreeAccumulator[Option[Tree]] {

      def foldTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] =
        if (x.isDefined) x
        else foldOverTree(x, tree)(owner)

      override def foldOverTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] = {
        tree match
          case t: Definition =>
            if (t.symbol == Symbol.noSymbol) {
              Some(t)
            } else
              // searh deep
              // x
              super.foldOverTree(x, t)(owner)
          case _ =>
            super.foldOverTree(x, tree)(owner)
      }
    }
    search.foldTree(None, tree)(Symbol.spliceOwner)

  class DefinitionWithIncorrectOwner(using quotes: Quotes)(
      tree: quotes.reflect.Definition,
      owner: quotes.reflect.Symbol,
      expectedOwner: quotes.reflect.Symbol,
      contextOwner: quotes.reflect.Symbol
  ) {
    def show: String =
      s"Incorrect owner for ${tree.symbol}(${tree.symbol.hashCode}): real owner $owner(${owner.hashCode()}), expected owner: ${expectedOwner}(${expectedOwner.hashCode}, contextOwner: ${contextOwner}(${contextOwner.hashCode})"
  }

  def findSubtermWithIncorrectOwner(using
      Quotes
  )(currentOwner: quotes.reflect.Symbol, tree: quotes.reflect.Tree): Option[DefinitionWithIncorrectOwner] =
    import quotes.reflect._
    import util._
    val search = new TreeAccumulator[(Symbol, Option[DefinitionWithIncorrectOwner])] {

      def foldTree(x: (Symbol, Option[DefinitionWithIncorrectOwner]), tree: Tree)(
          owner: Symbol
      ): (Symbol, Option[DefinitionWithIncorrectOwner]) =
        if (x._2.isDefined) x
        else foldOverTree(x, tree)(owner)

      def checkOwner(sym: Symbol, owner: Symbol): Boolean =
        (sym.maybeOwner == owner)

      override def foldOverTree(x: (Symbol, Option[DefinitionWithIncorrectOwner]), tree: Tree)(
          owner: Symbol
      ): (Symbol, Option[DefinitionWithIncorrectOwner]) = {
        tree match
          case t: Definition =>
            if (checkOwner(t.symbol, owner)) {
              // searh deep
              super.foldOverTree((t.symbol, None), t)(owner)
            } else {
              (x._1, Some(new DefinitionWithIncorrectOwner(t, t.symbol.maybeOwner, x._1, owner)))
            }
          case _ =>
            super.foldOverTree(x, tree)(owner)
      }
    }
    search.foldTree((currentOwner, None), tree)(Symbol.spliceOwner)._2

  // used for debugging instrumentation
  def dummyMapper(using Quotes)(t: quotes.reflect.Term, owner: quotes.reflect.Symbol): Boolean =
    import quotes.reflect._
    var wasError = false
    val checker = new TreeMap() {

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        try {
          super.transformTerm(tree)(owner)
        } catch {
          case ex: Throwable =>
            if (!wasError)
              ex.printStackTrace()
              wasError = true
            throw ex
        }

    }
    checker.transformTerm(t)(owner)
    wasError

  // workarround on bug, when changeowner does not change owner of definition, if this definietion
  //   is not inside some otjer tree.
  def reallyChangeOwner(using Quotes)(tree: quotes.reflect.Tree, owner: quotes.reflect.Symbol): quotes.reflect.Tree =
    import quotes.reflect.*
    tree match
      case td: Definition =>
        if (td.symbol.maybeOwner != Symbol.noSymbol && td.symbol.maybeOwner != owner) then
          val tmpBlock = Block(td :: Nil, '{}.asTerm)
          tmpBlock.changeOwner(owner) match
            case Block(x :: Nil, other) => x
            case _ =>
              throw MacroError(s"imposible: changeOwner changed structure of block ${tmpBlock}", tmpBlock.asExpr)
        else td
      case other =>
        other.changeOwner(owner)

  def prependStatementToBlock(using Quotes)(st: quotes.reflect.Statement, block: quotes.reflect.Block): quotes.reflect.Block = {
    import quotes.reflect.*
    val retval = block match
      case lambda @ Lambda(params, body) =>
        Block(st :: Nil, lambda)
      case _ =>
        block.expr match
          case Closure(_, _) =>
            Block(st :: Nil, block)
          case _ =>
            Block(st :: block.statements, block.expr)
    // if (false) {   TODO: pass debug-level
    //  dummyMapper(retval,Symbol.spliceOwner)
    // }
    retval
  }

  def prependStatementsToBlock(using
      Quotes
  )(sts: List[quotes.reflect.Statement], block: quotes.reflect.Block): quotes.reflect.Block = {
    import quotes.reflect.*
    val retval = block.expr match
      case Closure(_, _) =>
        Block(sts, block)
      case _ =>
        Block(sts ++: block.statements, block.expr)
    // if (true) {  TODO: pass debug-level
    //  dummyMapper(retval,Symbol.spliceOwner)
    // }
    retval
  }

  def inMonadOrChild[F[_]: Type](using Quotes)(te: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    te.widen.asType match
      case '[F[r]] if te.widen <:< TypeRepr.of[F[r]] =>
        Some(TypeRepr.of[r])
      case _ =>
        val bc = te.baseClasses
        if (bc.isEmpty) then None
        else
          TypeRepr
            .of[F]
            .classSymbol
            .flatMap(fs =>
              if (bc.contains(fs)) {
                Some(te.baseType(fs))
              } else {
                // TODO: search up, add tracking  F-bound to prevent infinite loop.
                None
              }
            )
  }

  def prependStatementsToTerm(using
      Quotes
  )(statements: List[quotes.reflect.Statement], term: quotes.reflect.Term): quotes.reflect.Block = {
    import quotes.reflect.*
    term match
      case lambda @ Lambda(_, _) =>
        Block(statements, lambda)
      case block @ Block(_, _) =>
        prependStatementsToBlock(statements, block)
      case _ =>
        Block(statements, term)
  }

  def lambdaBodyOwner(using Quotes)(lambda: quotes.reflect.Term): quotes.reflect.Symbol = {
    import quotes.reflect.*
    lambda match
      case Block((d: DefDef) :: Nil, Closure(_, _)) => d.symbol
      case Inlined(call, binding, body)             => lambdaBodyOwner(body)
      case _                                        => Symbol.noSymbol
  }

  /** @param fun
    *   expression, part of (Apply(fun, args)). Extract function symbols from carried applications: Apply( ... Apply(fun1, args1),
    *   ..., argsN) => fun1
    * @return
    *   extracted function symbol, (or just fun.symbol is this is not a carried application).
    */
  def extractCarriedFunSym(using Quotes)(fun: quotes.reflect.Term): quotes.reflect.Symbol = {
    import quotes.reflect.*
    fun match
      case Block((d: DefDef) :: Nil, Closure(_, _)) => d.symbol
      case Inlined(call, binding, body)             => extractCarriedFunSym(body)
      case TypeApply(Select(obj, "apply"), targs) if obj.tpe.widen.isFunctionType || obj.tpe.widen.isContextFunctionType =>
        extractCarriedFunSym(obj)
      case Select(obj, "apply") if obj.tpe.widen.isFunctionType || obj.tpe.widen.isContextFunctionType =>
        extractCarriedFunSym(obj)
      case _ =>
        fun.symbol
  }
