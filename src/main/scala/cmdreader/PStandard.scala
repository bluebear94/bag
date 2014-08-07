package cmdreader

/**
 * Operator precedence constants.
 * Any operator class should, if applicable, use these constants instead of hardcoded values.
 * @author bluebear94
 */

object PStandard {
  val ADD_SUBT = 200
  val MULT_DIV = 400
  val UNARY = 500
  val EXPONENT = 600
  val PERMUTATION = 500
  val RELATION = 150
  val INDIRECTION = 1500
  val LOOKUP = 900
  val ASSIGN = 100
  val TERNARY = 150
  val CONJUNCTION = 100
  val DISJUNCTION = 50
  val DOUBLE_OP = 1600
  val SHIFT = 175
  val MAP = 2000
  val TRY = 25
}
