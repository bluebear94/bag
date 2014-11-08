package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OAdd extends CommandOperator {
  def getName(): String = "add"
  def getOpAlias() = "+"
  def isValidArg0(n: Int) = true
  def apply(args: Array[Type]): Type = {
    args.fold(
    if (!args.isEmpty && args(0).getType == 3) new TString("")
    else new THill(0L)
    )(MathUtil.add(_, _))
  }
  def getPrecedence() = PStandard.ADD_SUBT
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = Some(new TMountain(1))
  override def isPure = true
}
