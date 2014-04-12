package cmdreader.std

import cmdreader._
import types._
import util._
import gui._

class ValCopy extends Command {
  override def getName = "valcopy"
  override def isValidArg0(n: Int) = n <= 1
  override def apply(args: Array[Type]) = {
    val old = Global.vigilant
    if (args.length == 1) {
      val ne = args(0).toBoolean
      Global.vigilant = ne
      Main.println("Turned automatic value copying " + (if (ne) "on" else "off"))
    }
    BTI.bti(old)
  }
}