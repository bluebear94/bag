package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OAvg extends CommandOperator {
  override def getName(): String = "avg"
  override def getOpAlias() = "@"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    MathUtil.idivide(MathUtil.add(args(0), args(1)), TMountain(Global.TWO))
  }
  def getPrecedence() = PStandard.ADD_SUBT
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(BigInteger.ONE))
}