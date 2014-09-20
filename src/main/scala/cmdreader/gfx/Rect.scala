package cmdreader.gfx

import cmdreader._
import types._
import util._

class Rect extends Command {
  def getName() = "rect"
  def isValidArg0(n: Int) = n == 5
  def apply(args: Array[Type]) = {
    GFX.rect(GFX.getIntOrChoke(args(0)),
      GFX.getIntOrChoke(args(1)),
      GFX.getIntOrChoke(args(2)),
      GFX.getIntOrChoke(args(3)),
      args(4).toBoolean)
    TVoid.inst
  }
}
