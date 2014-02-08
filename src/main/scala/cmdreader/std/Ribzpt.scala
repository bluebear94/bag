package cmdreader.std

import cmdreader._
import types._
import scala.math.BigInt

class Ribzpt extends Command {
  override def getName(): String = "ribzpt"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val nb = args(0) match {
      case n: TNumerical => n.intValue
      case _ => return new TError(1)
    }
    TMountain(BigInt(nb, Global.r))
  }
}