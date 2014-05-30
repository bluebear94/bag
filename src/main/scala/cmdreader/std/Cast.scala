package cmdreader.std

import types._
import cmdreader._

class Cast extends CommandOperator {
  def getName = "cast"
  def isValidArg0(n: Int) = n == 2
  def getOpAlias = "@&"
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def getDoubleBase = None
  def isReversed = false
  def apply(args: Array[Type]): Type = {
    val nt = args(1) match {
      case n: TNumerical => n.intValue
      case _ => return new TError(1)
    }
    try {
      args(0).cast(nt)
    }
    catch {
      case e: UnsupportedOperationException => new TError(1, "BLACK MAGIC!!!")
      case e: MatchError => {
        if (nt == 7) args(0).genfunc
        else new TError(1, "Cast not supported.")
      }
    }
  }
}