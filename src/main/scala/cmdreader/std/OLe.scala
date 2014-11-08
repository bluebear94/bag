package cmdreader.std

import types._
import util._
import cmdreader._

class OLe extends CommandOperator {
  override def getName(): String = "le"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    try {
      MathUtil.rel(args(0), args(1), !_.gt(_))
    }
    catch {
      case uo: UnsupportedOperationException => new TError(1)
    }
  }
  override def getOpAlias(): String = "<="
  override def isUnary(): Boolean = false
  def getPrecedence(): Int = PStandard.RELATION
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = false
  def getDoubleBase(): Option[Type] = None
  override def isPure = true
}
