package cmdreader.std

import cmdreader._
import types._
import scala.collection.mutable._

class Yield extends CommandOperator {
  override def getName(): String = "yield"
  override def isValidArg0(n: Int): Boolean = n <= 4 && n >= 2
  override def apply(args: Array[Type]): Type = {
    val l = args.length
    val s = args(0).asInstanceOf[TNumerical].intValue
    val e = args(1).asInstanceOf[TNumerical].intValue
    val f: (Type => Type) = if (args.length > 2) {
      args(2) match {
        case h: FuncLike => ((t: Type) => h(Array[Type](t)))
        case _ => return new TError(1, "Third argument must be function")
      }
    } else ((t: Type) => t)
    val k = (args.length > 3) && args(3).toBoolean
    val b = List.tabulate(e - s)((x: Int) => f(TMountain(x + s)))
    if (k) new LLinked(b.to[ListBuffer]) else new LArray(b.to[ArrayBuffer])
  }
  override def getOpAlias(): String = "=>"
  override def getPrecedence(): Int = PStandard.MAP
  override def isReversed(): Boolean = false
  override def hasAssignmentEquiv(): Boolean = false
  override def getDoubleBase(): Option[Type] = None
}
