package cmdreader.gfx

import cmdreader.Command
import types._
import util._

class STC extends Command {
  def getName = "stc"
  def isValidArg0(n: Int) = n == 1
  def apply(args: Array[Type]) = {
    new TMountain(GFX.stc(args(0).toString).getRGB)
  }
}