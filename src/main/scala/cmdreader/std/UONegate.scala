package cmdreader.std

import cmdreader._
import types._
import util._

class UONegate extends CommandOperator {
  override def getName(): String = "negate"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.negate(args(0))
  override def getOpAlias(): String = "⁻"
  override def isUnary(): Boolean = true
  def getPrecedence(): Int = PStandard.UNARY
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = false
  def getDoubleBase(): Option[Type] = None
  override def isPure = true
}
