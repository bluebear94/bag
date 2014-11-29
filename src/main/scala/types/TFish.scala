package types

import util._
import java.math.BigInteger

/**
 * A class to define floating-point values.
 * @author bluebear94
 */
case class TFish(x: Double) extends TNumerical {
  def getVal(): Double = {
    x
  }
  def getType(): Int = {
    4
  }
  def toStringP: String = {
    var raw: String = x.toString()
    if (raw.startsWith("0") && x != 0.0) raw = raw.substring(1)
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
  override def hashCode = {
    if (x.isWhole) x.toInt
    else x.hashCode
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
  def equalsStrictly(that: Type) = {
    that match {
      case t: TFish => x == t.x
      case _ => false
    }
  }
  override def intValue(): Int = x.toInt
  override def longValue: Long = x.toLong
  override def doubleValue(): Double = x
  def >/< = TFish(x)
  def toBytecode: Array[Byte] = {
    val n = java.lang.Double.doubleToRawLongBits(x)
    MakeByteArrays.longToByteArray(n)
  }
  override def toBoolean = x != 0.0
}
