
package cmdreader.std

import cmdreader._
import types._
import util._

class Idx extends CommandOperator {
  def getName = "idx"
  def getOpAlias = "@%|"
  def isValidArg0(n: Int) = n == 2 || n == 3
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]): Type = {
    val si = if (args.length == 2) 0
    else args(2) match {
      case n: TNumerical => n.intValue
      case _ => return new TError(1, "Starting index must be number")
    }
    args(1) match {
      case TString(s) => args(0) match {
        case n: TNumerical => TMountain(s.indexOf(n.intValue.toChar, si))
      }
      case TByteString(s) => args(0) match {
        case n: TNumerical => TMountain(s.indexOf(n.intValue.toByte, si))
      }
      case _ => TMountain(CollectionOps.ctv(_.indexOf(args(0), si))(args(1)))
    }
  }
  override def isPure = true
}
