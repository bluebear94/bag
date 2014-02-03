package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._
import util._

class FoldR extends Command {
  override def getName(): String = "foldr"
  override def isValidArg0(n: Int): Boolean = n == 3
  override def apply(args: Array[Type]): Type = {
    val collection = args(0)
    val base = args(1)
    val f: TFunction = args(2) match {
      case func: TFunction => func
      case _ => return new TError(1)
    }
    CollectionOps.ctv[Type](_.foldRight(base)((a, b) => f(Array(a, b))))(collection)
  }
}