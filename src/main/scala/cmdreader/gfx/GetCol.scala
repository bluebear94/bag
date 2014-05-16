package cmdreader.gfx

import types._
import cmdreader.Command
import util._

class GetCol extends Command {
  override def getName(): String = "getcol"
  override def isValidArg0(n: Int): Boolean = n <= 1
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  override def apply(args: Array[Type]): Type = {
    if (args.length == 0 || !args(0).toBoolean) new TMountain(GFX.getcol.getRGB & 0x00FFFFFF)
    else new TMountain(GFX.getcol.getRGB)
  }
}