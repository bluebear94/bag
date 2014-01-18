package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OOrB extends CommandOperator {
  override def getName(): String = "bor"
  override def getOpAlias() = "|"
  override def isValidArg0(n: Int) = n >= 1
  override def apply(args: Array[Type]): Type = {
    args.tail.fold(args.head)(MathUtil.bitOr(_, _))
  }
  def getPrecedence() = PStandard.DISJUNCTION
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = None
}