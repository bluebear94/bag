package cmdreader.std

import cmdreader._
import types._

class Rrbzo extends Command {
  override def getName(): String = "rrbzo"
  override def isValidArg0(n: Int): Boolean = n == 0
  override def apply(args: Array[Type]): Type = {
    TFish(Global.r.nextDouble)
  }
}