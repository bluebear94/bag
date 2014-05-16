
package cmdreader.std

import cmdreader._
import types._
import util._

class Isc extends CommandOperator {
  def getName = "isc"
  def getOpAlias = "@%*"
  def isValidArg0(n: Int) = n == 2
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]): Type = {
    BTI.bti(CollectionOps.ctv(_.containsSlice(CollectionOps.decodeToList(args(0))))(args(1)))
  }
}