package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OMult extends CommandOperator {
  def getName(): String = "mult"
  def getOpAlias() = "*"
  def isValidArg0(n: Int) = true
  def apply(args: Array[Type]): Type = {
    args.fold(
    new THill(1L)
    )(MathUtil.multiply(_, _))
  }
  def getPrecedence() = PStandard.MULT_DIV
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(new BigInteger("2")))
  override def isPure = true
}
