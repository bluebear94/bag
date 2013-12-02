package types

abstract class TFunction {
  def getType(): Int = {
    7
  }
  def apply(args: List[Type]): Type
  def equals(that: Type): Boolean = false
  def toBoolean(): Boolean = true
  // Implementations coming soon!
}