package cmdreader.std

import cmdreader.Command
import types._

class ECar extends Command {
  override def getName(): String = "ecar"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val l = args(0)
    l match {
      case a: LList => a.l.last
      case s: TString => new THill(s.getVal.last)
      case _ => new TError(1)
    }
  }
  override def isPure = true
}
