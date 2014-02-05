package cmdreader.std
import types._
import scala.collection.mutable.ArrayBuffer
import util.sort._

class HSort extends Sort {
  override def isValidArg0(n: Int): Boolean = n == 2
  override def getName = "hsort"
  override def getOpAlias = "/^"
  override def apply(args: Array[Type]): Type = {
    val l: ArrayBuffer[Type] = args(0) match {
      case a: LArray => a.l.to[ArrayBuffer]
      case _ => return new TError(1)
    }
    val f = args(1) match {
      case fn: TFunction => fn
      case _ => return new TError(1)
    }
    new LArray(Sorter.heapsort(l, (a, b) => f(Array(a, b)).toBoolean))
  }
}