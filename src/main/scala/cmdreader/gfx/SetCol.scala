package cmdreader.gfx

import types._
import cmdreader.Command
import util._
import java.awt._

class SetCol extends Command {
  override def getName(): String = "setcol"
  override def isValidArg0(n: Int): Boolean = n == 1 || n == 2
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  override def apply(args: Array[Type]): Type = {
    //new TMountain(GFX.getcol.getRGB)
    args(0) match {
      case x: TNumerical => {
        val i = if (args.length == 1 || !args(1).toBoolean) x.intValue | 0xFF000000 else x.intValue
        GFX.setcol(new Color(i))
        new TVoid
      }
      case _ => new TError(1, "Color must be number")
    }
  }
}