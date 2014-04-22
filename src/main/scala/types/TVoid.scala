package types

/**
 * A void value.
 */
class TVoid extends Type {

  def canEqual(that: Any): Boolean = {
    that.isInstanceOf[TVoid]
  }
  override def equals(that: Any): Boolean = {
    that match {
      case other: TVoid => true
      case _ => false
    }
  }
  def equalsStrictly(that: Type): Boolean = that.getType == 0
  def getType(): Int = {
    0
  }
  def toBoolean(): Boolean = {
    false
  }
  override def toString(): String = {
    "Void"
  }
  def toBytecode: Array[Byte] = {
    Array()
  }
  def >/< = new TVoid
}