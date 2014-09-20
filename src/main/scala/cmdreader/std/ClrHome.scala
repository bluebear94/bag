package cmdreader.std

import types._
import gui.Main
import cmdreader._

class ClrHome extends Command {
  def getName(): String = "clrHome"
  def isValidArg0(n: Int): Boolean = n == 0
  def apply(args: Array[Type]): Type = {
    Main.clrHome()
    TVoid.inst
  }
}
