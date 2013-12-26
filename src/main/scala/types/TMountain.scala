package types

import java.math.BigInteger
import util._

case class TMountain(n: BigInteger) extends TNumerical {
  
  def getVal(): BigInteger = {
    n
  }
  def getType(): Int = {
    1
  }
  override def toString(): String = {
    n.toString()
  }
  def equalsT(that: Type): Boolean = {
    val tt: Int = that.getType()
        (tt) match {
          case 1 => that.asInstanceOf[TMountain].getVal().equals(n)
          case 2 => n.equals(that.asInstanceOf[THill].getVal())
          case 4 => n.equals(that.asInstanceOf[TFish].getVal())
          case _ => false
        }
  }
  override def gt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
          case 1 => BigIntOps.gt(n, that.asInstanceOf[TMountain].getVal())
          case 2 => BigIntOps.gt(n, new BigInteger(
              that.asInstanceOf[THill].toString()
          ))
          case 4 => n.doubleValue() >
          that.asInstanceOf[TFish].getVal()
          case _ => throw new UnsupportedOperationException()
        }
  }
  override def lt(that: Type): Boolean = {
    val tt: Int = that.getType()
    (tt) match {
          case 1 => BigIntOps.lt(n, that.asInstanceOf[TMountain].getVal())
          case 2 => BigIntOps.lt(n, new BigInteger(
              that.asInstanceOf[THill].toString()
          ))
          case 4 => n.doubleValue() <
          that.asInstanceOf[TFish].getVal()
          case _ => throw new UnsupportedOperationException()
        }
  }
  def intValue(): Int = n.intValue()
  def si(i: Int, b: Boolean) = {
    if (b) n.setBit(i)
    else n.clearBit(i)
  }
  def >/< = TMountain(new BigInteger(n.toString))
  def toBytecode: Array[Byte] = {
    val s = n.toByteArray
    MakeByteArrays.intToByteArray(s.length) ++ s
  }
}