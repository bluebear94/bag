package types

import util._
import java.math.BigInteger


/**
 * A class to define 64-bit integers.
 * @author bluebear94
 */
case class THill(var n: Long) extends TNumerical {
  def getVal(): Long = {
    n
  }
  def getType(): Int = {
    2
  }
  def toStringP: String = n.toString + "H"
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
  override def hashCode = n.hashCode
  override def gt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => that.asInstanceOf[TMountain].getVal() < n
      case 2 => n > that.asInstanceOf[THill].getVal()
      case 4 => n > that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  override def lt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
      case 1 => that.asInstanceOf[TMountain].getVal() > n
      case 2 => n < that.asInstanceOf[THill].getVal()
      case 4 => n < that.asInstanceOf[TFish].getVal()
      case _ => throw new UnsupportedOperationException()
    }
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: THill => n == t.n
      case _ => false
    }
  }
  override def intValue(): Int = n.toInt
  override def doubleValue(): Double = n.toDouble
  def snv(nn: Long) = n = nn
  def >/< = THill(n)
  def toBytecode: Array[Byte] = {
    MakeByteArrays.longToByteArray(n)
  }
  override def toBoolean = n != 0
}
