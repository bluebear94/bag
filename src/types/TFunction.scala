package types

abstract class TFunction extends Type {
  def getType(): Int = {
    7
  }
  def apply(args: Array[Type]): Type
  def equals(that: Type): Boolean = false
  def toBoolean(): Boolean = true
  override def toString(): String = "a function"
  // Implementations coming soon!
}