package cmdreader.std

import types._
import util._
import cmdreader._

class OSeq extends CommandOperator {
  override def getName(): String = "seq"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    BTI.bti(args(0).equalsStrictly(args(1)))
  }
  override def getOpAlias(): String = "==="
  override def isUnary(): Boolean = false
  def getPrecedence(): Int = PStandard.RELATION
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = false
  def getDoubleBase(): Option[Type] = None
}