package types

import util.BigIntOps
import java.math.BigInteger

case class THill(n2: Long) extends TNumerical {
  var n = n2
  def getVal(): Long = {
    n
  }
  def getType(): Int = {
    2
  }
  override def toString(): String = {
    "↼" + n.toString()
  }
  def equalsT(that: Type): Boolean = {
    val tt: Int = that.getType()
    n.equals(
        (tt) match {
          case 1 => that.asInstanceOf[TMountain].getVal().longValue
          case 2 => that.asInstanceOf[THill].getVal()
          case 4 => that.asInstanceOf[TFish].getVal().toLong
          case _ => false
        }
        )
  }
  override def gt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 =>
        BigIntOps.lt(that.asInstanceOf[TMountain].getVal(),
            new BigInteger(toString()))
      case 2 => n > that.asInstanceOf[THill].getVal()
      case 4 => n > that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  override def lt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 =>
        BigIntOps.gt(that.asInstanceOf[TMountain].getVal(),
            new BigInteger(toString()))
      case 2 => n < that.asInstanceOf[THill].getVal()
      case 4 => n < that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  def intValue(): Int = n.toInt
  def snv(nn: Long) = n = nn
  def >/< = THill(n)
  def toBytecode: Array[Byte] = {
    var a = new Array[Byte](8)
    for (i <- 0 until 7) {
      a(8 - i) = (n & (0xFFL << 8 * i) >> 8 * i).toByte
    }
    a
  }
  override def toBoolean = n != 0
}