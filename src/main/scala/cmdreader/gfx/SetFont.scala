package cmdreader.gfx

import types._
import cmdreader.Command
import util._
import java.awt._

class SetFont extends Command {
  override def getName(): String = "setf"
  override def isValidArg0(n: Int): Boolean = n == 1
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  override def apply(args: Array[Type]): Type = {
    //new TMountain(GFX.getcol.getRGB)
    args(0) match {
      case x: LArray => {
        GFX.setf(GFX.deserializeFont(x))
        new TVoid
      }
      case _ => new TError(1, "Font setting must be array in format {name, style, size}")
    }
  }
}