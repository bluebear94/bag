
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
    (args(0), args(1)) match {
      case (needle: TNumerical, TString(haystack)) => BTI.bti(haystack contains needle.intValue.toChar)
      case (needle: TNumerical, TByteString(haystack)) => BTI.bti(haystack contains needle.intValue.toByte)
      case (needle, haystack) => BTI.bti(CollectionOps.ctv(_ contains needle)(haystack))
    }
  }
  override def isPure = true
}
