package cmdreader.gfx

import cmdreader._
import types._
import util._

class PtOn extends Command {
  def getName() = "ptOn"
  def isValidArg0(n: Int) = n == 2
  def apply(args: Array[Type]) = {
    GFX.ptOn(GFX.getIntOrChoke(args(0)),
      GFX.getIntOrChoke(args(1)))
    new TVoid
  }
}
