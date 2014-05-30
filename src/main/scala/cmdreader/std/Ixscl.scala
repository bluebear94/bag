
package cmdreader.std

import cmdreader._
import types._
import util._

class Ixscl extends CommandOperator {
  def getName = "ixscl"
  def getOpAlias = "@%*|-"
  def isValidArg0(n: Int) = n == 2 || n == 3
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]): Type = {
    val slice = CollectionOps.decodeToList(args(0))
    val l = CollectionOps.decodeToList(args(1)).length
    if (args.length == 2)
      TMountain(CollectionOps.ctv(l - 1 - _.indexOfSlice(slice))(args(1)))
    else {
      val si = args(2) match {
        case n: TNumerical => n.intValue
        case _ => return new TError(1, "Starting index must be number")
      }
      TMountain(CollectionOps.ctv(l - 1 - _.indexOfSlice(slice, l - 1 - si))(args(1)))
    }
  }
}