package cmdreader.std

import cmdreader._
import types._

class Prepare extends Command {
  override def getName(): String = "prepare"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    TString(args(0).toString.replaceAll("\\[", "[lb]").replaceAll("\\]", "[rb"))
  }
  override def isPure = true
}
