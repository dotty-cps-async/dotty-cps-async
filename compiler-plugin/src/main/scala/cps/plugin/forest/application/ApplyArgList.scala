package cps.plugin.forest.application

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*
import cps.plugin.*
import cps.plugin.forest.*
import dotty.tools.dotc.ast.tpd

sealed trait ApplyArgList {
  def isAsync(using Context, CpsTopLevelContext): Boolean
  def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext): Boolean
  def containsAsyncLambda(using Context, CpsTopLevelContext): Boolean
  def findDirectContext: Option[Tree]
  def containsDirectContext: Boolean
  def show(using Context): String
  def origin: Tree
  def isTypeParams: Boolean
  def isPlainParams: Boolean

  /** The arguments with vals for sync arguments, which are not pure paths (see `ApplyArg.withSyncBinding`). */
  def withSyncBindings(using Context, CpsTopLevelContext): ApplyArgList
}

case class ApplyTermArgList(
    originApplyTerm: Apply,
    args: List[ApplyArg]
) extends ApplyArgList {
  override def isAsync(using Context, CpsTopLevelContext) = args.exists(_.isAsync)
  override def containsAsyncLambda(using Context, CpsTopLevelContext) = args.exists(_.isAsyncLambda)
  override def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext) =
    args.exists(x => x.isAsyncLambda && !x.lambdaCanBeUnshifted)
  override def findDirectContext: Option[Tree] = args.find(_.isDirectContext).map(_.origin)
  override def containsDirectContext = args.exists(_.isDirectContext)
  override def origin = originApplyTerm
  override def isTypeParams = false
  override def isPlainParams = true

  override def withSyncBindings(using Context, CpsTopLevelContext): ApplyArgList =
    copy(args = args.map(_.withSyncBinding))

  override def show(using Context): String = {
    s"ApplyTermArgList(${args.map(_.show)})"
  }

}

case class ApplyTypeArgList(
    originApplyTerm: TypeApply,
    args: List[TypeTree]
) extends ApplyArgList {
  override def isAsync(using Context, CpsTopLevelContext) = false
  override def containsAsyncLambda(using Context, CpsTopLevelContext) = false
  override def containsNotUnshiftableAsyncLambda(using Context, CpsTopLevelContext) = false
  override def findDirectContext: Option[Tree] = None
  override def containsDirectContext = false
  override def origin = originApplyTerm
  override def show(using Context): String = {
    s"ApplyTypeArgList(${args})"
  }
  override def isTypeParams = true
  override def isPlainParams = false
  override def withSyncBindings(using Context, CpsTopLevelContext): ApplyArgList = this
}

object ApplyTermArgList {

  def make(term: Apply, mt: MethodParamsDescriptor, owner: Symbol, nesting: Int)(using
      Context,
      CpsTopLevelContext
  ): ApplyTermArgList = {
    val args = term.args.zipWithIndex.map { (a, index) =>
      val (aExpr, named) = a match
        case NamedArg(name, expr) => (expr, Some(name.toTermName))
        case expr                 => (expr, None)
      ApplyArg(
        aExpr,
        mt.paramName(index, a.srcPos).toTermName,
        mt.paramType(index, a.srcPos),
        mt.isByName(index, a.srcPos),
        mt.isDirectContext(index, a.srcPos),
        owner,
        named,
        nesting
      )
    }
    ApplyTermArgList(term, args)
  }

}
