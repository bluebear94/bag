package types
/*
 * Ahh. The type.
 * A fundamental part of the Amethyst language.
 */
abstract class Type {

  def getType(): Int
  def toBoolean(): Boolean
  def equals(that: Type): Boolean
  override def toString(): String
}


