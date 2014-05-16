package cmdreader.gfx

import cmdreader.Command
import types._
import util._

class STC extends Command {
  def getName = "stc"
  def isValidArg0(n: Int) = n == 1 || n == 2
  def apply(args: Array[Type]) = {
    if (args.length == 1 || !args(1).toBoolean) new TMountain(GFX.stc(args(0).toString).getRGB & 0x00FFFFFF)
    else new TMountain(GFX.stc(args(0).toString).getRGB)
  }
}