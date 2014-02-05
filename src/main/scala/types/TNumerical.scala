package types

/**
 * A class to define numbers.
 * @author bluebear94
 */
abstract class TNumerical extends Type {
  /**
   * Gets the value of this type.
   */
  def getVal(): Any
  def toBoolean(): Boolean = {
    getVal() == 0
  }
  /**
   * Gets the 32-bit integer value of this type.
   */
  def intValue(): Int
  /**
   * Gets the 64-bit floating-point value of this type.
   */
  def doubleValue(): Double
  def equalsT(that: Type): Boolean
  override def equals(that: Any): Boolean = {
    that match {
      case other: TNumerical => equalsT(other)
      case _ => false
    }
  }
}