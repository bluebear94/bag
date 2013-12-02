package types

class THill(n: Long) extends TNumerical {
  def getVal(): Long = {
    n
  }
  def getType(): Int = {
    2
  }
  override def toString(): String = {
    "#" + n.toString()
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
}