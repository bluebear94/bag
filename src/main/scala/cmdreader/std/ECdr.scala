package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class ECdr extends Command {
  override def getName(): String = "ecdr"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val l = args(0)
    l match {
      case a: LArray => new LArray(a.l.init.to[ArrayBuffer])
      case a: LLinked => new LLinked(a.l.init.to[ListBuffer])
      case s: TString => new TString(s.getVal.init)
      case _ => new TError(1)
    }
  }
  override def isPure = true
}
