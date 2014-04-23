package types

/**
 * An abstract representation of a function.
 */
abstract class TFunction extends Type {
  def getType(): Int = {
    7
  }
  def apply(args: Array[Type]): Type
  override def equals(that: Any): Boolean = false
  override def equalsStrictly(that: Any) = equals(that)
  def toBoolean(): Boolean = true
  override def toString(): String = "a function"
    //TODO method stub
  def toBytecode = Array[Byte]()
}