package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OSubt extends CommandOperator {
  override def getName(): String = "subt"
  override def getOpAlias() = "-"
  override def apply(args: Array[Type]): Type = {
    args.fold(
      THill(0L))(MathUtil.subtract(_, _))
  }
  def getPrecedence() = PStandard.ADD_SUBT
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(BigInteger.ONE))
}