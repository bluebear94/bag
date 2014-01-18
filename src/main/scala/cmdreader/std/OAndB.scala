package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OAndB extends CommandOperator {
  override def getName(): String = "band"
  override def getOpAlias() = "&"
  override def isValidArg0(n: Int) = n >= 1
  override def apply(args: Array[Type]): Type = {
    args.tail.fold(args.head)(MathUtil.bitAnd(_, _))
  }
  def getPrecedence() = PStandard.CONJUNCTION
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = None
}