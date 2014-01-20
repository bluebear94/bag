package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OTt extends CommandOperator {
  override def getName(): String = "tt"
  override def getOpAlias() = "^"
  override def isValidArg0(n: Int) = n == 2
  override def apply(args: Array[Type]): Type = {
    args.tail.fold(
    args.head
    )(MathUtil.tt(_, _))
  }
  def getPrecedence() = PStandard.EXPONENT
  def isReversed() = true
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(new BigInteger("2")))
}