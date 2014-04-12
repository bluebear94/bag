package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class Clone extends Command {
  override def getName(): String = "clone"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    args(0).>/<
  }
}