package parse.ast

trait Bin
case class Byte(b: Byte) extends Bin
case class Bytes(bs: Array[Byte]) extends Bin
case class Offset(n: Int) extends Bin
case class Label(n: String) extends Bin
case class Goto(n: String) extends Bin
object BFuncs {
  
}