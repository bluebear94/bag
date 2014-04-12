package types

import cmdreader.Global

/**
 * An abstract class to define instances of an Amethyst type.
 * @author bluebear94
 */

abstract class Type {
  /**
   * Returns the type identifier of the instance.
   * @return the type ID
   */
  def getType(): Int
  /**
   * Returns the implied Boolean value of the instance.
   * @return false if 0, ↼0, "", 0.0, {}, or []; true otherwise
   */
  def toBoolean(): Boolean
  def equals(that: Any): Boolean
  /**
   * Returns whether one instance is greater than another.
   * @param that the instance of a type to which to compare
   * @throws UnsupportedOperationException when such operation is not supported
   */
  def gt(that: Type): Boolean = {
    throw new UnsupportedOperationException()
  }
  /**
   * Returns whether one instance is less than another.
   * @param that the instance of a type to which to compare
   * @throws UnsupportedOperationException when such operation is not supported
   */
  def lt(that: Type): Boolean = {
    throw new UnsupportedOperationException()
  }
  override def toString(): String
  /**
   * Returns a clone of this type.
   */
  def >/<(): Type
  def cid: Type = if (Global.vigilant) this.>/< else this
  /**
   * Returns the binary representation of this type.
   */
  def toBytecode: Array[Byte]
}


