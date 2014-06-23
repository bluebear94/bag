package cmdreader.std

import types._
import cmdreader.Command
import math._
import util.MathUtil

class Itsb extends Command {
  override def getName(): String = "itsb"
  override def isValidArg0(n: Int): Boolean = n == 2
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  override def apply(args: Array[Type]): Type = {
    val _s = args(0)
    val s = _s match {
      case s: TNumerical => s.getVal
      case _ => return new TError(1, "#1 must be number")
    }
    val _b = args(1)
    val b = _b match {
      case b: TNumerical => b.intValue
      case _ => return new TError(1, "#2 must be number")
    }
    if (b <= 0) new TError(6, "Base must be at least 2")
    else new TString(s match {
      case s: BigInt => s.toString(b)
      case s: Long => BigInt(s).toString(b)
      case s: Double => MathUtil.toStringRadix(s, b)
    })
  }
}
