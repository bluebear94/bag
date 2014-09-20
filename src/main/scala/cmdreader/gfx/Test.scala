package cmdreader.gfx

import cmdreader.Command
import types._
import util._

class Test extends Command {
  def getName(): String = "test"
  def isValidArg0(n: Int): Boolean = n == 0
  def apply(args: Array[Type]) = {
    GFX.test()
    TVoid.inst
  }
}
