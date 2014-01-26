package cmdreader.std

import cmdreader._
import types._
import util._
import java.math.BigInteger

class OMap extends CommandOperator {
  override def getName(): String = "map"
  override def getOpAlias() = "+>"
  override def isValidArg0(n: Int) = n >= 2
  override def apply(args: Array[Type]): Type = {
    val argc = args.length
    val lists = args.dropRight(1)
    val f0 = args(argc - 1)
    val f = f0 match {
      case f: TFunction => f
      case _ => return new TError(1, f0 + "is not a function")
    }
    val trueLists = lists.map(CollectionOps.decodeToList(_))
    val flipped = TurnOnSide(trueLists)
    val newList = flipped.map(f(_))
    CollectionOps.encodeFromList(newList, lists(0).getType)
  }
  def getPrecedence() = PStandard.MAP
  def isReversed() = false
  def hasAssignmentEquiv() = true
  def getDoubleBase() = None
}