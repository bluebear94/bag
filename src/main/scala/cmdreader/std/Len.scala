package cmdreader.std

import cmdreader.Command
import util._
import types._

class Len extends Command {
  override def getName(): String = "len"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    new TMountain(CollectionOps.ctv(_.length)(args(0)))
  }
}