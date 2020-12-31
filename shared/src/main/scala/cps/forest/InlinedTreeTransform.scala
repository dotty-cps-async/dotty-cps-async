// CPS Transform for tasty inlined
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._
import scala.collection.immutable.HashMap


trait InlinedTreeTransform[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.reflect._

  sealed trait InlinedBindingRecord

  case class InlinedFunBindingRecord(newSym: Symbol, 
                                     cpsTree: CpsTree, 
                                     oldValDef: ValDef, 
                                     newResultType: TypeRepr) extends InlinedBindingRecord

  case class InlinedValBindingRecord(newSym: Symbol, 
                                     cpsTree: CpsTree, 
                                     oldValDef: ValDef,
                                     awaitNewSym: Symbol) extends InlinedBindingRecord

  case class InlinedBindingsRecord(changes: HashMap[Symbol, InlinedBindingRecord], 
                                   newBindings: List[Definition], 
                                   awaitVals: List[ValDef])
  
  def runInlined(origin: Inlined): CpsTree =
    if (cpsCtx.flags.debugLevel >= 15) then
        cpsCtx.log(s"Inlined, origin=${origin.show}")  
    val s0 = InlinedBindingsRecord(HashMap.empty, List.empty, List.empty)
    val funValDefs = origin.bindings.zipWithIndex.foldLeft(s0){ (s, xi) =>
       val (x,i) = xi
       x match
          case vx@ValDef(name,tpt,Some(rhs)) =>
              checkLambdaDef(rhs) match
                 case Some(lambda) => 
                    lambda match
                      case Lambda(params, body) =>
                         val cpsBinding = runRoot(body, TransformationContextMarker.InlinedBinding(i))
                         val resultType = vx.tpt.tpe match
                             case AppliedType(fun, args) => args.last
                             case _ => body.tpe.widen
                         if (cpsBinding.isAsync) then
                            val lambdaTree = new AsyncLambdaCpsTree(lambda, params, cpsBinding, resultType)
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, lambdaTree.rtpe,  vx.symbol.flags, Symbol.noSymbol)
                            val rLambda = lambdaTree.rLambda.changeOwner(newSym)
                            val newValDef = ValDef(newSym, Some(rLambda))
                            val bindingRecord = InlinedFunBindingRecord(newSym, lambdaTree, vx, resultType) 
                            s.copy(
                               changes = s.changes.updated(vx.symbol, bindingRecord),
                               newBindings = newValDef::s.newBindings
                            )
                         else if (cpsBinding.isChanged) then
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, tpt.tpe,  vx.symbol.flags, Symbol.noSymbol)
                            val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => resultType)
                            val newValDef = ValDef(newSym, cpsBinding.syncOrigin.map(body =>
                               Lambda(Symbol.spliceOwner, mt, 
                                 (owner,args)=> TransformUtil.substituteLambdaParams(params,args,body,owner).changeOwner(owner)
                               )
                            ))
                            val bindingRecord = InlinedFunBindingRecord(newSym, CpsTree.pure(newValDef.rhs.get), vx, resultType) 
                            s.copy(
                               changes = s.changes.updated(vx.symbol, bindingRecord),
                               newBindings = newValDef::s.newBindings
                            ) 
                         else 
                            // unchanged
                            s.copy(newBindings = vx::s.newBindings)
                      case _ => // impossible
                         s.copy(newBindings = vx::s.newBindings)
                 case None => 
                    val cpsRhs = runRoot(rhs, TransformationContextMarker.InlinedBinding(i))
                    /*
                    if (cpsRhs.isAsync) {
                         ??? 
                    } else if (cpsRhs.isChanged) {
                         ???
                    } else {
                        s.copy(newBindings = vx::s.newBindings)
                    }
                    */
                    s.copy(newBindings = vx::s.newBindings)
          case nonValDef => 
               s.copy(newBindings = x::s.newBindings)
    }  
    val body =
      if (!funValDefs.changes.isEmpty) {
        val monad = cpsCtx.monad.asTerm
        val transformer = new TreeMap() {

             override def transformTerm(term:Term)(owner: Symbol): Term =
               try
                 transformTermInternal(term)(owner)
               catch
                 case ex: MacroError =>
                   if (cpsCtx.flags.debugLevel > 0) then
                     report.warning(s"failed term: ${term}")
                   throw ex

             def transformTermInternal(term:Term)(owner: Symbol): Term =
               // TODO: implements other methods then apply, i.e. anThen, compose, etc
               term match
                 case Apply(TypeApply(Select(obj@Ident(name),"apply"),targs),args) =>
                        funValDefs.changes.get(obj.symbol) match
                           case Some(binding: InlinedFunBindingRecord) =>
                              val newArgs = args.map(a => this.transformTerm(a)(owner))
                              val newApply = Select.unique(Ref(binding.newSym),"apply")
                              val changed = Apply(TypeApply(newApply,targs),newArgs)
                              if (binding.cpsTree.isAsync) then
                                 Apply(Apply(TypeApply(Ref(awaitSymbol),List(TypeTree.of[F])),List(changed)),List(monad))
                              else
                                 changed
                           case None =>
                              super.transformTerm(term)(owner)
                 case Apply(Select(obj@Ident(name),"apply"),args) =>
                        funValDefs.changes.get(obj.symbol) match
                           case Some(binding: InlinedFunBindingRecord) =>
                              val newArgs = args.map(a => this.transformTerm(a)(owner))
                              val newApply = Select.unique(Ref(binding.newSym),"apply")
                              val changed = Apply(newApply,newArgs)
                              if (binding.cpsTree.isAsync) then
                                 Apply(Apply(TypeApply(Ref(awaitSymbol),List(TypeTree.of[F])),List(changed)),List(monad))
                              else
                                 changed
                           case None =>
                              super.transformTerm(term)(owner)
                 case obj@Ident(name) => 
                        funValDefs.changes.get(obj.symbol) match
                           case Some(vbinding) =>
                             vbinding match
                               case binding: InlinedFunBindingRecord =>
                                 if (binding.cpsTree.isAsync) then
                                   // inline themself
                                   //   low priority todo: think about escaping of work duplication if inlining.
                                   //TODO: currently inline flag is not set in proxy. 
                                   val inlineProxies = binding.oldValDef.symbol.flags.is(Flags.Inline)
                                   val rhs = binding.oldValDef.rhs.get
                                   checkLambdaDef(rhs) match
                                     case Some(Lambda(params, body)) =>
                                        val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => binding.newResultType)
                                        Lambda(Symbol.spliceOwner, mt, 
                                              (owner,args) => 
                                                  (if inlineProxies then
                                                      TransformUtil.substituteLambdaParams(params,args,body,owner)
                                                   else
                                                      Apply(Select.unique(Ref(binding.newSym),"apply"),
                                                                 args.map(_.asInstanceOf[Term]))
                                                  ).changeOwner(owner)
                                        )
                                     case _ =>
                                       throw MacroError("Lambda in rhs expected",posExprs(rhs))
                                 else
                                   Ref(binding.newSym)
                               case binding: InlinedValBindingRecord =>
                                 ???
                           case None =>
                              super.transformTerm(term)(owner)
                 case _ =>
                        super.transformTerm(term)(owner)
        }
        transformer.transformTerm(origin.body)(Symbol.spliceOwner)
      } else {
        origin.body
      }
    if (cpsCtx.flags.debugLevel >= 15) then
        cpsCtx.log(s"runInline, body=${body}")
        cpsCtx.log(s"runInline, newBindings=${funValDefs.newBindings.map(_.show).mkString("\n")}")
        funValDefs.changes.foreach{ b =>
           cpsCtx.log(s"fubValDef changes binding: ${b}")
        } 
    val cpsBody = runRoot(body, TransformationContextMarker.InlinedBody)
    if (origin.bindings.isEmpty) then
       cpsBody
    else
       InlinedCpsTree(origin, funValDefs.newBindings.reverse,  cpsBody)


  def checkLambdaDef(term:Term):Option[Term] =
     term match
        case Block(List(),expr) => checkLambdaDef(expr)
        case lt@Lambda(params,body) => Some(lt)
        case _ => None


object InlinedTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         inlinedTerm: qctx1.reflect.Inlined): CpsExpr[F,T] = {

     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            val origin = inlinedTerm.asInstanceOf[quotes.reflect.Inlined]
            runInlined(origin).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }



