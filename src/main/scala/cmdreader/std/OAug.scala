package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OAug extends CommandOperator {
  override def getName(): String = "aug"
  override def getOpAlias() = "&+"
  override def isValidArg0(n: Int) = n >= 2
  override def apply(args: Array[Type]): Type = {
    val trueLists = args.map(CollectionOps.decodeToList(_))
    val newList = trueLists.fold(Nil)(_ ++ _)
    CollectionOps.encodeFromList(newList, args(0).getType)
  }
  def getPrecedence() = PStandard.ADD_SUBT
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = None
}