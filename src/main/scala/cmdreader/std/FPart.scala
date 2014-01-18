package cmdreader.std

import cmdreader.Command
import types._
import gui._
import util._

class FPart extends Command {
  override def getName(): String = "fpart"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    MathUtil.fpart(args(0))
  }
}