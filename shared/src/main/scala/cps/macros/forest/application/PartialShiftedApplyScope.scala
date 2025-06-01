package cps.macros.forest.application

import cps.*
import cps.macros.forest.*

enum ApplicationShiftType:
  case CPS_ONLY
  case CPS_RUNTIME_AWAIT
  case CPS_DEFERR_TO_PLUGIN

case class PartialShiftedApplyFlags(
    useExtraArguments: Boolean = false,
    isExtensionMethod: Boolean = false,
    typeParamsListIndex: Int = -1
)

object PartialShiftedApplyFlags:

  def empty: PartialShiftedApplyFlags = PartialShiftedApplyFlags()

trait PartialShiftedApplyScope[F[_], CT, CC <: CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  /** Application with one list of params.
    */
  case class PartialShiftedApply(
      shiftType: ApplicationShiftType,

      /** function, which will be applied. the argument is `runtimeAwait`, which is needed when type of shift is CPS_AWAIT. when
        * type of shift is CPS_ONLY or SPS_DEFERR_TO_PLUGIN, runtimeAwait is not used (and can be emoty term)
        */
      shiftedDelayed: Term => Term,
      applyFlags: PartialShiftedApplyFlags
  ):

    def withTailArgs(argTails: List[ApplyArgsList], withAsync: Boolean): Term => Term = {
      def appliedToArgsOrTypeArgs(
          fun: Term,
          argTails: List[ApplyArgsList],
          argTransform: ApplyArgRecord => Term
      ): Term = {
        val s0: (Term, Boolean) = (fun, false)
        val s = argTails.foldLeft(s0) { case ((term, fWasAdded), e) =>
          e match
            case ApplyTermArgsList(originApply, args) =>
              (term.appliedToArgs(args.map(argTransform).toList), fWasAdded)
            case ApplyTypeArgsList(originApply) =>
              if applyFlags.isExtensionMethod && applyFlags.useExtraArguments && !fWasAdded then
                // add extra type parameter and arguemnt with monad to the shifted function
                //  (when extension method is used, first argument is the 'self' of extension method,
                //    so we shpuld modify the second argument lists, both type and value)
                val extraTypeArg = TypeTree.of[F]
                val typed = TypeApply.copy(originApply)(term, extraTypeArg :: originApply.args)
                val extraArg = cpsCtx.monad.asTerm
                val withNewArg = Apply.copy(originApply)(typed, List(extraArg))
                (withNewArg, true)
              else
                // if function was not applied, then we can use original apply term
                // to avoid double apply
                (TypeApply.copy(originApply)(term, originApply.args), fWasAdded)
        }
        s._1
      }

      runtimeAwait => {
        val tailArgTransform = shiftType match
          case ApplicationShiftType.CPS_ONLY =>
            (arg: ApplyArgRecord) => arg.shift().identArg(withAsync)
          case ApplicationShiftType.CPS_RUNTIME_AWAIT =>
            (arg: ApplyArgRecord) => arg.withRuntimeAwait(runtimeAwait).identArg(withAsync)
          case ApplicationShiftType.CPS_DEFERR_TO_PLUGIN =>
            (arg: ApplyArgRecord) => arg.term
        appliedToArgsOrTypeArgs(shiftedDelayed(runtimeAwait), argTails, tailArgTransform)
      }
    }
