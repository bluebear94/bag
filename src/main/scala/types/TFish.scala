package types

import util.BigIntOps
import java.math.BigInteger

case class TFish(x: Double) extends TNumerical {
  def getVal(): Double = {
    x
  }
  def getType(): Int = {
    4
  }
  override def toString(): String = {
    var raw: String = x.toString()
    if (raw.startsWith("0")) raw = raw.substring(1)
    if (raw.endsWith("0")) raw = raw.substring(0, raw.length() - 1)
    raw
  }
  def equalsT(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => that.asInstanceOf[TMountain].getVal().doubleValue.equals(x)
      case 2 => x == that.asInstanceOf[THill].getVal()
      case 4 => x == that.asInstanceOf[TFish].getVal()
      case _ => false
    }
  }
  override def gt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => x > that.asInstanceOf[TMountain].getVal().doubleValue()
      case 2 => x > that.asInstanceOf[THill].getVal()
      case 4 => x > that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  override def lt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => x < that.asInstanceOf[TMountain].getVal().doubleValue()
      case 2 => x < that.asInstanceOf[THill].getVal()
      case 4 => x < that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  def intValue(): Int = x.toInt
  def >/< = TFish(x)
  def toBytecode: Array[Byte] = {
    val n = java.lang.Double.doubleToRawLongBits(x)
    var a = new Array[Byte](8)
    for (i <- 0 until 7) {
      a(8 - i) = (n & (0xFFL << 8 * i) >> 8 * i).toByte
    }
    a
  }
}