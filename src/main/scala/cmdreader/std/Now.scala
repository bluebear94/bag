package cmdreader.std

import cmdreader.Command
import types._
import java.util.Date

class Now extends Command {
  override def getName(): String = "now"
  override def isValidArg0(n: Int) = n == 0
  override def apply(args: Array[Type]): Type = {
    new THill(new Date().getTime)
  }
}