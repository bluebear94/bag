
package cmdreader.std

import cmdreader._
import types._
import util._
import scala.collection.mutable.ArrayBuffer

class Idxe extends CommandOperator {
  def getName = "idx"
  def getOpAlias = "@%|*"
  def isValidArg0(n: Int) = n == 2 || n == 4
  def getPrecedence = PStandard.RELATION
  def hasAssignmentEquiv = false
  def isReversed = false
  def getDoubleBase = None
  def apply(args: Array[Type]): Type = {
    val occurrences =
      CollectionOps.ctv(CollectionOps.findAllOccurrences(args(0), _))(args(1))
    val of = if (args.length == 4) {
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