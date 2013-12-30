package parse.ast

import scala.collection.mutable._

trait Bin
case class AByte(b: Byte) extends Bin
case class Bytes(bs: Array[Byte]) extends Bin
case class Offset(n: Int) extends Bin
case class BLabel(n: String) extends Bin
case class BGoto(n: String) extends Bin
object BFuncs {
  def enlen(b: Bin) = {
    b match {
      case AByte(_) => 1
      case Bytes(a) => a.length
      case Offset(_) => 4
      case BLabel(_) =>  0
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
}