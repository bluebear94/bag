package cmdreader.std

import types._
import util._
import cmdreader._

class OEq extends CommandOperator {
  def getName(): String = "eq"
  def isValidArg0(n: Int): Boolean = n == 2
  def apply(args: Array[Type]): Type = {
    MathUtil.rel(args(0), args(1), _.equals(_))
  }
  def getOpAlias(): String = "=="
  override def isUnary(): Boolean = false
  def getPrecedence(): Int = PStandard.RELATION
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = false
  def getDoubleBase(): Option[Type] = None
  override def isPure = true
}
