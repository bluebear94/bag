package cmdreader.gfx

import types._
import cmdreader.Command
import util._

class FromHSV extends Command {
  override def getName(): String = "fromHSV"
  override def isValidArg0(n: Int): Boolean = n == 3
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  override def apply(args: Array[Type]): Type = {
    TMountain(GFX.fromHSV(args(0).intValue, args(1).intValue, args(2).intValue))
  }
}
