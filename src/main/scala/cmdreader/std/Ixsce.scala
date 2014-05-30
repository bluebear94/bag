
package cmdreader.std

import cmdreader._
import types._
import util._
import scala.collection.mutable.ArrayBuffer

class Ixsce extends CommandOperator {
  def getName = "ixsce"
  def getOpAlias = "@%*|*"
  def isValidArg0(n: Int) = n >= 2 && n < 6
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]): Type = {
    val slice = CollectionOps.decodeToList(args(0))
    val overlap = args.length % 2 == 0 || args(args.length - 1).toBoolean
    val occurrences =
      CollectionOps.ctv(CollectionOps.findAllOccurrences(slice, _, overlap))(args(1))
    val of = if (args.length >= 4) {
      val si = args(2) match {
        case n: TNumerical => n.intValue
        case _ => return new TError(1, "Starting index must be number")
      }
      val ei = args(3) match {
        case n: TNumerical => n.intValue
        case _ => return new TError(1, "Starting index must be number")
      }
      occurrences.filter(a => a >= si && a < ei)
    } else occurrences
    new LArray(of.map((x: Int) => TMountain(BigInt(x))).toBuffer.to[ArrayBuffer])
  }
}