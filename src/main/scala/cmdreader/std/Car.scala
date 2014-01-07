package cmdreader.std

import cmdreader.Command
import types._

class Car extends Command {
  override def getName(): String = "car"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val l = args(0)
    l match {
      case a: LList => a.l.head
      case s: TString => new THill(s.getVal.charAt(0))
      case _ => new TError(1)
    }
  }
}