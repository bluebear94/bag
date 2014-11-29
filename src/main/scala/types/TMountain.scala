package types

import scala.math.BigInt
import util._

/**
 * A type to define arbitrary-precision integers.
 * @author bluebear94
 */
case class TMountain(var n: BigInt) extends TNumerical {
  def getVal(): BigInt = {
    n
  }
  def getType(): Int = {
    1
  }
  def toStringP: String = n.toString
  def equalsT(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => that.asInstanceOf[TMountain].getVal().equals(n)
      case 2 => n.longValue == that.asInstanceOf[THill].getVal
      case 4 => n.doubleValue == that.asInstanceOf[TFish].getVal
      case _ => false
    }
  }
  override def hashCode = n.hashCode
  override def gt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => n > that.asInstanceOf[TMountain].getVal
      case 2 => n > that.asInstanceOf[THill].getVal
      case 4 => n.doubleValue() >
        that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  override def lt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => n < that.asInstanceOf[TMountain].getVal()
      case 2 => n < that.asInstanceOf[THill].getVal
      case 4 => n.doubleValue() <
        that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: TMountain => n == t.n
      case _ => false
    }
  }
  override def toBoolean = n != 0
  override def intValue(): Int = n.intValue()
  override def longValue: Long = n.longValue
  override def doubleValue(): Double = n.doubleValue()
  def si(i: Int, b: Boolean) = {
    n = if (b) n.setBit(i)
    else n.clearBit(i)
  }
  def >/< = TMountain(n)
  def toBytecode: Array[Byte] = {
    n.toByteArray
  }
}
