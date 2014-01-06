package parse.ast

import scala.collection.mutable._
import util.MakeByteArrays

trait Bin
case class AByte(b: Byte) extends Bin
object AByte {
  implicit def byteToAByte(b: Byte) = AByte(b)
}
case class Bytes(bs: Array[Byte]) extends Bin
object Bytes {
  implicit def byteArrayToBytes(b: Array[Bytes]) = Array(b)
}
case class Offset(n: Int) extends Bin
case class BLabel(n: String) extends Bin
case class BGoto(n: String) extends Bin
object BFuncs {
  def enlen(b: Bin) = {
    b match {
      case AByte(_) => 1
      case Bytes(a) => a.length
      case Offset(_) => 4
      case BLabel(_) => 0
      case BGoto(_) => 6
    }
  }
  def alen(b: Array[Bin]) = {
    b.foldLeft(0)((l, n) => l + enlen(n))
  }
  def app(a0: Array[Bin], a1: Array[Bin]) = {
    val l = a0.length
    val b1 = a1.map(_ match {
      case Offset(n) => Offset(n + l)
      case r => r
    })
    a0 ++ b1
  }
  def flatten(b: Array[Bin]) = {
    var table = new HashMap[String, Int]
    var requests = new HashMap[Int, String]
    var na = Array[Byte]()
    for (i <- b) {
      i match {
        case AByte(b) => na = na :+ b
        case Bytes(b) => na = na ++ b
        case Offset(n) => na = na ++ MakeByteArrays.intToByteArray(n)
        case BLabel(s) => table(s) = na.length
        case BGoto(s) => {
          na = na ++ Array[Byte](-0x17, 0x34, 0, 0, 0, 0)
          requests(na.length - 4) = s
        }
      }
    }
    for (i <- requests.toList) {
      i match {
        case (addr, ln) => {
          val jaddr = table(ln)
          val asa = MakeByteArrays.intToByteArray(jaddr)
          for (i <- 0 until 4) {
            na(addr + i) = asa(i)
          }
        }
      }
    }
    na
  }
  def bytecodeToString(b: Array[Byte]) = {
    b.map({ n =>
      val p = if (n >= 0) n else 0x100 + n
      def ohd(n: Int) = (if (n < 10) '0' + n else 55 + n).toChar
      new String(Array(ohd(p >> 4), ohd(p & 0x0F)))
    }).foldLeft("")(_ + " " + _)
  }
}