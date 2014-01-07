package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class Cdr extends Command {
  override def getName(): String = "cdr"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val l = args(0)
    l match {
      case a: LArray => new LArray(a.l.tail.to[ArrayBuffer])
      case a: LLinked => new LLinked(a.l.tail.to[ListBuffer])
      case s: TString => new TString(s.getVal.substring(1))
      case _ => new TError(1)
    }
  }
}