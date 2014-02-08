package cmdreader.std
import types._
import scala.collection.mutable.ListBuffer
import util.sort._

class MSort extends Sort {
  override def isValidArg0(n: Int): Boolean = n == 2
  override def getName = "msort"
  override def getOpAlias = "/<"
  override def apply(args: Array[Type]): Type = {
    val l: ListBuffer[Type] = args(0) match {
      case k: LLinked => k.l.to[ListBuffer]
      case _ => return new TError(1)
    }
    val f = args(1) match {
      case fn: TFunction => fn
      case _ => return new TError(1)
    }
    new LLinked(Sorter.mergesort(l, (a, b) => f(Array(a, b)).toBoolean))
  }
}