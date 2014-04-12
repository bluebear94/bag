package cmdreader.std

import cmdreader._
import types._
import gui._

class HAsk extends Command {
  override def getName(): String = "hask"
  override def isValidArg0(n: Int): Boolean = n == 0
  override def apply(args: Array[Type]): Type = {
    new TString(Main.ask)
  }
}