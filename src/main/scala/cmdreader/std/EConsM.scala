package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class EConsM extends Command {
  override def getName(): String = "econs!"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    val t = args(0)
    val l = args(1)
    try {
      l match {
        case a: LList => a.l += t
        case s: TByteString => s.a = s.a :+ t.byteValue
        case s: TString => {
          s.s = s.s + new String(Array[Char](
            t match {
              case n: TMountain => n.getVal.longValue.toChar
              case n: THill => n.getVal.toChar
              case n: TFish => n.getVal.toChar
              case _ => throw new RuntimeException
            }))
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
