package cps.plugin.forest.application

enum ShiftedArgumentsTypeParamsShape {
  case SAME_TYPEPARAMS, EXTRA_TYPEPARAM_IN_FIRST_TYPEPARAM_LIST, EXTRA_TYPEPARAM_IN_SECOND_TYPEPARAM_LIST, EXTRA_TYPEPARAM_LIST
}

enum ShiftedArgumentsPlainParamsShape {
  case SAME_PARAMS, EXTRA_FIRST_PARAM_IN_FIRST_PARAMLIST, EXTRA_FIRST_PARAM_IN_SECOND_PARAMLIST, EXTRA_PARAM_LIST
}

case class ShiftedArgumentsShape(
    tp: ShiftedArgumentsTypeParamsShape,
    p: ShiftedArgumentsPlainParamsShape
)

object ShiftedArgumentsShape {

  def same = ShiftedArgumentsShape(
    ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS,
    ShiftedArgumentsPlainParamsShape.SAME_PARAMS
  )

  def extraLists = ShiftedArgumentsShape(
    ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM_LIST,
    ShiftedArgumentsPlainParamsShape.EXTRA_PARAM_LIST
  )

}
