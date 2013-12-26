package types

abstract class TFunction extends Type {
  def getType(): Int = {
    7
  }
  def apply(args: Array[Type]): Type
  override def equals(that: Any): Boolean = false
  def toBoolean(): Boolean = true
  override def toString(): String = "a function"
    //TODO method stub
  def toBytecode = Array[Byte]()
}