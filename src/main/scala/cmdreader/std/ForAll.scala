package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._
import util._

class ForAll extends Command {
  override def getName(): String = "all"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    val collection = args(0)
    val f: FuncLike = args(1) match {
      case func: FuncLike => func
      case _ => return new TError(1)
    }
    CollectionOps.ctv[Type]((l: List[Type]) => BTI.bti(l forall { (x: Type) =>
      f(Array(x)).toBoolean
    }))(collection)
  }
}
