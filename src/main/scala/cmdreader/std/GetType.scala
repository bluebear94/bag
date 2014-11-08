package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class GetType extends Command {
  override def getName(): String = "gettype"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    TMountain(args(0).getType)
  }
  override def isPure = true
}
