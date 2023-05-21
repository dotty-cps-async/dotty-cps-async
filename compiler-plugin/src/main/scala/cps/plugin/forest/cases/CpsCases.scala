package cps.plugin.forest.cases

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*

import cps.plugin.*
import cps.plugin.forest.*


class CpsCases(val cases: List[CpsCaseDef]) {

  lazy val collectAsyncKind: AsyncKind =
    cases.foldLeft(AsyncKind.Sync) { (acc, c) =>
      acc.unify(c.cpsBody.asyncKind) match
        case Right(x) => x
        case Left(msg) => throw CpsTransformException("Can't unify async shape in case branches for match", c.origin.srcPos)
    }

  def  transformedCaseDefs(targedKind:AsyncKind)(using Context, CpsTopLevelContext): List[CaseDef] =
    cases.map(_.transform(targedKind))

  def unchanged(using Context, CpsTopLevelContext): Boolean =
    cases.forall(_.cpsBody.isOriginEqSync)

}

object CpsCases {

  def create(cases: List[CaseDef], owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsCases =
      val cpsCases = cases.map( c => CpsCaseDef(c,RootTransform(c.body,owner,nesting+1)) )
      CpsCases(cpsCases)


}
