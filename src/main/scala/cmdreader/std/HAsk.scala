package cmdreader.std

import cmdreader._
import types._
import gui._

class HAsk extends Command {
  override def getName(): String = "hask"
  override def isValidArg0(n: Int): Boolean = n == 0
  override def apply(args: Array[Type]): Type = {
    Main.msg = null
    var ticks = 0
    Main.setSt(Main.ASKING)
    while (Main.msg == null) {
      ticks += 1
    }
    Main.setSt(Main.BUSY)
    val res = Main.msg.toString
    Main.println(res)
    new TString(res)
  }
}