package cmdreader.std

import cmdreader._
import types._

class Version extends Command {
  override def getName(): String = "version"
  override def isValidArg0(n: Int) = n == 0
  override def apply(args: Array[Type]): Type = {
    TMountain(Global.vint)
  }
}
