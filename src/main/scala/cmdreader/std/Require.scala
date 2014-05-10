package cmdreader.std

import cmdreader._
import types._
import run._

class Require extends Command {
  override def getName(): String = "require"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    args(0) match {
      case s: TString => {
        Global.loadLib(s.getVal)
        new TVoid
      }
      case _ => new TError(1, "library name must be string")
    }
  }
}