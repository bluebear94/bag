package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class ConsM extends Command {
  override def getName(): String = "cons!"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    val t = args(0)
    val l = args(1)
    try {
      l match {
        case a: LList => t +=: a.l
        case s: TByteString => s.a = t.byteValue +: s.a
        case s: TString => {
          s.s = new String(Array[Char](
            t match {
              case n: TMountain => n.getVal.longValue.toChar
              case n: THill => n.getVal.toChar
              case n: TFish => n.getVal.toChar
              case _ => throw new RuntimeException
            })) + s.getVal
        }
        case _ => return new TError(1)
      }
    }
    catch {
      case e: RuntimeException => return new TError(1)
    }
    TVoid
  }
  override def protocol = FProtocol.single
}
