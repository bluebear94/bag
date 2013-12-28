package cmdreader

import types.Type

abstract class CommandOperator extends Command {
  //require(isValidArg0(if (isUnary()) 1 else 2))
  def getOpAlias(): String = ""
  def isUnary(): Boolean = false
  def getPrecedence(): Int
  def isReversed(): Boolean
  def hasAssignmentEquiv(): Boolean
  def getDoubleBase(): Option[Type]
}