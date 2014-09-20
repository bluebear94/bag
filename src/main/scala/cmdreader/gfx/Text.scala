package cmdreader.gfx

import cmdreader._
import types._
import util._

class Text extends Command {
  def getName() = "text"
  def isValidArg0(n: Int) = n == 3
  def apply(args: Array[Type]) = {
    GFX.text(args(0).toString,
      GFX.getIntOrChoke(args(1)),
      GFX.getIntOrChoke(args(2)))
    TVoid.inst
  }
}
