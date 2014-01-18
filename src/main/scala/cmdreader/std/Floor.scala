package cmdreader.std

import cmdreader.Command
import types._
import gui._
import util._

// What TI-Basic-inspired language could be complete without a floor function?

class Floor extends Command {
  override def getName(): String = "floor"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    MathUtil.floor(args(0))
  }
}