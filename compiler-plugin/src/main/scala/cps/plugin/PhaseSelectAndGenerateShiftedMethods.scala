package cps.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Symbols.*
import core.Types.*
import core.Decorators.toTermName
import plugins.*
import cps.plugin.DefDefSelectKind.USING_CONTEXT_PARAM
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import dotty.tools.dotc.core.DenotTransformers.{DenotTransformer, IdentityDenotTransformer, InfoTransformer}
import dotty.tools.dotc.transform.{Pickler, SetRootTree}

class PhaseSelectAndGenerateShiftedMethods(selectedNodes: SelectedNodes) extends PluginPhase with IdentityDenotTransformer {

  val phaseName = PhaseSelectAndGenerateShiftedMethods.phaseName

  override val runsAfter = Set(SetRootTree.name)
  override val runsBefore = Set(Pickler.name, PhaseCps.name)

  override def changesMembers: Boolean = true
  override def changesParents: Boolean = true

  // Allow implicit search in this phase for CpsPreprocessor lookup.
  // Preprocessing must happen here (before Inlining) so the inline macro can be expanded.
  override def allowsImplicitSearch: Boolean = true

  // Ensure the context has this phase for implicit search to work
  override def prepareForDefDef(tree: tpd.DefDef)(using Context): Context = {
    super.prepareForDefDef(tree).withPhase(this)
  }

  override def prepareForTemplate(tree: tpd.Template)(using Context): Context = {
    super.prepareForTemplate(tree).withPhase(this)
  }

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    tree.rhs match
      case EmptyTree =>
        tree
      case other =>
        if (
            !tree.symbol.flags.isOneOf(Flags.InlineOrProxy | Flags.Synthetic) &&
            CpsTransformHelper.isCpsDirectType(tree.rhs.tpe)
          )
        then
          report.error("CpsDirect can't be a value", tree.srcPos)
          tree
        else super.transformValDef(tree)
  }

  override def transformAssign(tree: tpd.Assign)(using Context): tpd.Tree = {
    if (CpsTransformHelper.isCpsDirectType(tree.tpe)) then
      report.error("CpsDirect can't be a assigned", tree.srcPos)
      tree
    else super.transformAssign(tree)
  }

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {

    // annotate selected methods with CpsTransform and apply preprocessing if needed
    val preprocessedBody = tree.body.map { m =>
      annotateAndPreprocessMethod(m)
    }

    // add shifted methods for @makeCPS annotated high-order members
    val makeCpsAnnot = Symbols.requiredClass("cps.plugin.annotation.makeCPS")
    val shiftedMethods = preprocessedBody
      .filter(_.symbol.annotations.exists(_.symbol == makeCpsAnnot))
      .flatMap { m =>
        m match
          case dd: DefDef => ShiftedMethodGenerator.generateShiftedMethod(dd)
          case other =>
            report.error("Only DefDef can be annotated by @makeCPS", other.srcPos)
            None
      }

    val bodyChanged = preprocessedBody.zip(tree.body).exists((a, b) => a ne b)

    if (shiftedMethods.isEmpty && !bodyChanged) then tree
    else
      for (m <- shiftedMethods) {
        m.symbol.enteredAfter(this)
      }
      cpy.Template(tree)(body = preprocessedBody ++ shiftedMethods)
  }

  /**
   * Annotate method with CpsTransformed and apply preprocessing if CpsPreprocessor exists.
   * Returns the (possibly modified) tree.
   */
  def annotateAndPreprocessMethod(tree: tpd.Tree)(using Context): tpd.Tree = {
    lazy val cpsTransformedAnnot = Symbols.requiredClass("cps.plugin.annotation.CpsTransformed")
    tree match
      case dd: DefDef if (!dd.symbol.hasAnnotation(Symbols.requiredClass("cps.plugin.annotation.CpsNotChange"))) =>
        val optKind = SelectedNodes.detectDefDefSelectKind(dd)
        optKind match
          case Some(kind) =>
            val monadType =
              CpsTransformHelper.extractMonadType(kind.getCpsDirectContext.tpe, CpsTransformHelper.cpsDirectAliasSymbol, dd.srcPos)
            val annotExpr = New(cpsTransformedAnnot.typeRef.appliedTo(monadType), Nil)
            val initAnnotExpr = Apply(TypeApply(Select(annotExpr, "<init>".toTermName), List(TypeTree(monadType))), Nil)
            dd.symbol.addAnnotation(ConcreteAnnotation(initAnnotExpr))
            selectedNodes.addDefDef(dd.symbol, kind)

            // Apply preprocessing if CpsPreprocessor[F] exists.
            // This must happen here (before Inlining phase) so the inline macro gets expanded.
            if (dd.rhs.isEmpty) then
              dd
            else
              CpsTransformHelper.findCpsPreprocessor(monadType, dd.rhs.span) match
                case Some(preprocessor) =>
                  // Wrap: body → preprocessor.preprocess(body)
                  // The inline macro will be expanded during Inlining phase
                  val preprocessMethod = Select(preprocessor, "preprocess".toTermName)
                  val preprocessedRhs = Apply(
                    TypeApply(preprocessMethod, List(TypeTree(dd.rhs.tpe.widen))),
                    List(dd.rhs)
                  )
                  // Mark the compilation unit as needing inlining since we inserted an inline call
                  summon[Context].compilationUnit.needsInlining = true
                  cpy.DefDef(dd)(rhs = preprocessedRhs)
                case None =>
                  dd
          case None => tree
      case _ =>
        tree
  }

  // TODO: This method is currently unused since annotateAndPreprocessMethod now handles both
  // annotation and preprocessing. Keep for now in case we need to revert.
  def annotateTopMethodWithSelectKind(tree: tpd.Tree)(using Context): Boolean = {
    lazy val cpsTransformedAnnot = Symbols.requiredClass("cps.plugin.annotation.CpsTransformed")
    tree match
      case dd: DefDef if (!dd.symbol.hasAnnotation(Symbols.requiredClass("cps.plugin.annotation.CpsNotChange"))) =>
        val optKind = SelectedNodes.detectDefDefSelectKind(dd)
        optKind match
          case Some(kind) =>
            val monadType =
              CpsTransformHelper.extractMonadType(kind.getCpsDirectContext.tpe, CpsTransformHelper.cpsDirectAliasSymbol, dd.srcPos)
            val annotExpr = New(cpsTransformedAnnot.typeRef.appliedTo(monadType), Nil)
            val initAnnotExpr = Apply(TypeApply(Select(annotExpr, "<init>".toTermName), List(TypeTree(monadType))), Nil)
            dd.symbol.addAnnotation(ConcreteAnnotation(initAnnotExpr))
            selectedNodes.addDefDef(dd.symbol, kind)
            true
          case None => false
      case _ =>
        false
  }

}

object PhaseSelectAndGenerateShiftedMethods {

  val phaseName = "rssh.cpsSelect"

}
