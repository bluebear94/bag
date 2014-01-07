package cmdreader.std

import cmdreader._
import types._
import util._

class UONot extends CommandOperator {
  override def getName(): String = "not"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.not(args(0))
  override def getOpAlias(): String = "!"
  override def isUnary(): Boolean = true
  def getPrecedence(): Int = PStandard.UNARY
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = false
  def getDoubleBase(): Option[Type] = None
}