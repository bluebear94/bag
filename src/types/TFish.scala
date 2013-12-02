package types

class TFish(x: Double) extends TNumerical {
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
  def equals(that: Type): Boolean = {
    val tt: Int = that.getType()
    x.equals(
        (tt) match {
          case 1 => that.asInstanceOf[TMountain].getVal().equals(x)
          case 2 => x == that.asInstanceOf[THill].getVal()
          case 4 => x == that.asInstanceOf[TFish].getVal()
          case _ => false
        }
        )
  }
}