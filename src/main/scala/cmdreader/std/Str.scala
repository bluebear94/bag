package cmdreader.std

import cmdreader.Command
import types._

class Str extends Command {
  override def getName(): String = "str"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    TString(args(0).toString)
  }
}