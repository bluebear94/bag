package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OAnd extends CommandOperator {
  override def getName(): String = "and"
  override def getOpAlias() = "&'"
  override def apply(args: Array[Type]): Type = {
    BTI.bti(args.map(_.toBoolean).fold(true)(_ && _))
  }
  def getPrecedence() = PStandard.CONJUNCTION
  def isReversed() = false
  def hasAssignmentEquiv() = false
  def getDoubleBase() = None
}