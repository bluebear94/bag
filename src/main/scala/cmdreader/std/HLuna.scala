package cmdreader.std

import cmdreader.Command
import types._
import gui._

class HLuna extends Command {
  override def getName(): String = "hluna"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    println("[hluna] " + args(0))
    Main.println(args(0).toString)
    TVoid.inst
  }
}
