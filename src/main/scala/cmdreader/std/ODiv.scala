package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class ODiv extends CommandOperator {
  override def getName(): String = "div"
  override def getOpAlias() = "/"
  override def apply(args: Array[Type]): Type = {
    args.tail.fold(args.head)(MathUtil.divide(_, _))
  }
  def getPrecedence() = PStandard.MULT_DIV
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(2))
}