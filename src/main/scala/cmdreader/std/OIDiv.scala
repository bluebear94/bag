package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OIDiv extends CommandOperator {
  override def getName(): String = "idiv"
  override def getOpAlias() = "\\"
  override def apply(args: Array[Type]): Type = {
    args.tail.fold(args.head)(MathUtil.idivide(_, _))
  }
  def getPrecedence() = PStandard.MULT_DIV
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(Global.TWO))
}