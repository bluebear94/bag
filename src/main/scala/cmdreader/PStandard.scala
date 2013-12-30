package cmdreader

// Operator precedence for standard operators.

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
}