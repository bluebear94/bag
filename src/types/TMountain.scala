package types

import java.math.BigInteger

class TMountain(n: BigInteger) extends TNumerical {
  
  def getVal(): BigInteger = {
    n
  }
  def getType(): Int = {
    1
  }
  override def toString(): String = {
    n.toString()
  }
  def equals(that: Type): Boolean = {
    val tt: Int = that.getType()
    n.equals(
        (tt) match {
          case 1 => that.asInstanceOf[TMountain].getVal().equals(n)
          case 2 => n.equals(that.asInstanceOf[THill].getVal())
          case 4 => n.equals(that.asInstanceOf[TFish].getVal())
          case _ => false
        }
        )
  }
}