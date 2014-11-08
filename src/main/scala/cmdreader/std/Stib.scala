package cmdreader.std

import types._
import cmdreader.Command
import math._

class Stib extends Command {
  override def getName(): String = "stib"
  override def isValidArg0(n: Int): Boolean = n == 2
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  override def apply(args: Array[Type]): Type = {
    val _s = args(0)
    val s = _s match {
      case s: TString => s.s
      case _ => return new TError(1, "#1 must be string")
    }
    val _b = args(1)
    val b = _b match {
      case b: TNumerical => b.intValue
      case _ => return new TError(1, "#2 must be number")
    }
    val i = s.indexOf('.')
    if (b <= 0) new TError(6, "Base must be at least 2")
    else if (i == -1) new TMountain(BigInt(s, b))
    else {
      val ip = BigInt(s.substring(0, i), b)
      new TFish((BigDecimal(ip) +
        BigDecimal(BigInt(s.substring(i + 1), b)) * ip.signum / BigDecimal(b).pow(s.length - i - 1)).floatValue)
    }
  }
  override def isPure = true
}
