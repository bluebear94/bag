package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class ORor extends CommandOperator {
  override def getName(): String = "ror"
  override def getOpAlias() = ">>>"
  override def isValidArg0(n: Int) = n == 2
  override def apply(args: Array[Type]): Type = {
    MathUtil.ror(args(0), args(1))
  }
  def getPrecedence() = PStandard.SHIFT
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = None
  override def isPure = true
}
