package types

import util.BigIntOps
import java.math.BigInteger

class THill(n: Long) extends TNumerical {
  def getVal(): Long = {
    n
  }
  def getType(): Int = {
    2
  }
  override def toString(): String = {
    "↼" + n.toString()
  }
  def equals(that: Type): Boolean = {
    val tt: Int = that.getType()
    n.equals(
        (tt) match {
          case 1 => that.asInstanceOf[TMountain].getVal().equals(n)
          case 2 => that.asInstanceOf[THill].getVal().equals(n)
          case 4 => that.asInstanceOf[TFish].getVal().equals(n)
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
}