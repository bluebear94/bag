package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OIDiv extends CommandOperator {
  def getName(): String = "idiv"
  def getOpAlias() = "\\"
  def isValidArg0(n: Int) = n >= 1
  def apply(args: Array[Type]): Type = {
    args.tail.fold(args.head)(MathUtil.idivide(_, _))
  }
  def getPrecedence() = PStandard.MULT_DIV
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(2))
  override def isPure = true
}
