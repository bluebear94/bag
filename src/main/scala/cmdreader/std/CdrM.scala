package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class CdrM extends Command {
  override def getName(): String = "cdr!"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val l = args(0)
    l match {
      case a: LList => a.l.trimStart(1)
      case a: TByteString => a.a = a.a drop 1
      case s: TString => s.s = s.getVal.substring(1)
      case _ => return new TError(1)
    }
    TVoid
  }
  override def protocol = FProtocol.single
}
