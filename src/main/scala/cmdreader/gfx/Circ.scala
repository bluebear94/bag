package cmdreader.gfx

import cmdreader._
import types._
import util._

class Circ extends Command {
  def getName() = "circ"
  def isValidArg0(n: Int) = n == 4
  def apply(args: Array[Type]) = {
    GFX.circ(GFX.getIntOrChoke(args(0)),
      GFX.getIntOrChoke(args(1)),
      GFX.getIntOrChoke(args(2)),
      args(3).toBoolean)
    new TVoid
  }
}