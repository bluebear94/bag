package cmdreader.std

import cmdreader.Command
import types._
import gui._
import util._

class Abs extends Command {
  override def getName(): String = "abs"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    MathUtil.abs(args(0))
  }
}