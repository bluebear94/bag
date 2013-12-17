package types
/*
 * Ahh. The type.
 * A fundamental part of the Amethyst language.
 */
abstract class Type {

  def getType(): Int
  def toBoolean(): Boolean
  def equals(that: Type): Boolean
  def gt(that: Type): Boolean = {
    throw new UnsupportedOperationException()
  }
  def lt(that: Type): Boolean = {
    throw new UnsupportedOperationException()
  }
  override def toString(): String
  def >/<(): Type
}


