package types

class TVoid extends Type {

  def equals(that: Type): Boolean = {
    that.getType() == 0
  }
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