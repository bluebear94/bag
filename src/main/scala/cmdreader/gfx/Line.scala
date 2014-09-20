package cmdreader.gfx

import cmdreader._
import types._
import util._

class Line extends Command {
  def getName() = "line"
  def isValidArg0(n: Int) = n == 4
  def apply(args: Array[Type]) = {
    GFX.line(GFX.getIntOrChoke(args(0)),
      GFX.getIntOrChoke(args(1)),
      GFX.getIntOrChoke(args(2)),
      GFX.getIntOrChoke(args(3)))
    TVoid.inst
  }
}
