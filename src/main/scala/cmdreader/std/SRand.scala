package cmdreader.std

import cmdreader._
import types._
import scala.math.BigInt

class SRand extends Command {
  override def getName(): String = "srand"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val nb = args(0) match {
      case n: TNumerical => n.longValue
      case _ => return new TError(1)
    }
    Global.r.setSeed(nb)
    TVoid
  }
}
