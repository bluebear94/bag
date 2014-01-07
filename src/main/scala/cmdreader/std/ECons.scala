package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class ECons extends Command {
  override def getName(): String = "econs"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    val t = args(0)
    val l = args(1)
    try {
      l match {
        case a: LArray => new LArray((a.l :+ t).to[ArrayBuffer])
        case a: LLinked => new LLinked((a.l :+ t).tail.to[ListBuffer])
        case s: TString => {
          new TString(s.getVal + new String(Array[Char](
            t match {
              case n: TMountain => n.getVal.longValue.toChar
              case n: THill => n.getVal.toChar
              case n: TFish => n.getVal.toChar
              case _ => throw new RuntimeException
            })))
        }
        case _ => new TError(1)
      }
    }
    catch {
      case e: RuntimeException => new TError(1)
    }
  }
}