
package cmdreader.std

import cmdreader._
import types._
import util._

class Idxl extends CommandOperator {
  def getName = "idxl"
  def getOpAlias = "@%|-"
  def isValidArg0(n: Int) = n == 2 || n == 3
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]): Type = {
    val l = args(1) match {
      case TString(s) => s.length
      case TByteString(s) => s.length
      case _ => CollectionOps.decodeToList(args(1)).length
    }
    val si = if (args.length == 2) 0
    else args(2) match {
      case n: TNumerical => n.intValue
      case _ => return new TError(1, "Starting index must be number")
    }
    args(1) match {
      case TString(s) => args(0) match {
        case n: TNumerical => TMountain(l - 1 - s.reverse.indexOf(n.intValue.toChar, l - 1 - si))
      }
      case TByteString(s) => args(0) match {
        case n: TNumerical => TMountain(l - 1 - s.reverse.indexOf(n.intValue.toByte, l - 1 - si))
      }
      case _ => TMountain(CollectionOps.ctv(l - 1 - _.reverse.indexOf(args(0), l - 1 - si))(args(1)))
    }
  }
  override def isPure = true
}
