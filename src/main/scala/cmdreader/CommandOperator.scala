package cmdreader

import types.Type
/**
 * A class to represent an operator.
 */
abstract class CommandOperator extends Command {
  require(isValidArg0(if (isUnary()) 1 else 2))
  /**
   * Returns the operator symbol.
   */
  def getOpAlias(): String// = ""
  /**
   * Returns whether the operator is unary.
   */
  def isUnary(): Boolean = false
  /**
   * Returns the precedence of the operator.
   */
  def getPrecedence(): Int
  /**
   * Returns true if the operator is evaluated right-to-left, and false if left-to-right.
   */
  def isReversed(): Boolean
  /**
   * Returns whether the operator has a variant with the equals sign.
   */
  def hasAssignmentEquiv(): Boolean
  /**
   * Returns Some[t] if getOpAlias-getOpAlias variable is equivalent to variable getOpAlias= t; None otherwise.
   */
  def getDoubleBase(): Option[Type]
}