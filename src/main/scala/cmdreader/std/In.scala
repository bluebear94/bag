
package cmdreader.std

import cmdreader._
import types._
import util._

class In extends CommandOperator {
  def getName = "in"
  def getOpAlias = "@%"
  def isValidArg0(n: Int) = n == 2
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]) = {
    BTI.bti(CollectionOps.ctv(_.contains(args(0)))(args(1)))
  }
}