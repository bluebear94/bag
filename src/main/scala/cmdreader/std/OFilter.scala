package cmdreader.std

import cmdreader._
import types._
import scala.collection.mutable._
import util._

class OFilter extends CommandOperator {
  override def getName(): String = "filter"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    val collection = args(0)
    val f: FuncLike = args(1) match {
      case func: FuncLike => func
      case _ => return new TError(1)
    }
    CollectionOps.ctc(_.filter(a => f(Array(a)).toBoolean))(collection)
  }
  override def getOpAlias(): String = "|>"
  def getPrecedence(): Int = PStandard.MAP
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = true
  def getDoubleBase(): Option[Type] = None
}
