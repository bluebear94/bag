package types

import cmdreader.Global
import util._
import scala.collection.immutable.Set

/**
 * A trait to define instances of an Amethyst type.
 * @author bluebear94
 */

trait Type {
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
  /**
    Returns whether one instance strictly equals another.
  */
  def equalsStrictly(that: Any): Boolean = that match {
      case t: Type => equalsStrictly(t)
      case _ => false
    }
  def equalsStrictly(that: Type): Boolean
  override def toString(): String = if (Global.vigilant) toStringNC else toStringC(Set())
  def toStringNC(): String
  def toStringC_(visited: Set[Type]): String
  def toStringC(visited: Set[Type]): String = if (visited contains this) "..." else toStringC_(visited)
  /**
   * Returns a clone of this type.
   */
  def >/<(): Type
  def cid: Type = if (Global.vigilant) this.>/< else this
  /**
   * Returns the binary representation of this type.
   */
  def toBytecode: Array[Byte]
  def cast(i: Int): Type
  def genfunc = {
    val bc = toBytecode
    new TBinFunc(Array[Byte](-0x1f, getType.toByte) ++
            MakeByteArrays.intToByteArray(bc.length) ++ bc ++ Array[Byte](-0x17, 0x53))
  }
  
}

/**
  A trait for atomic types, useful for defining string functions that do not pose the risk of cycling.
  @author bluebear94
*/
trait Atom extends Type {
  def toStringP: String
  def toStringNC(): String = toStringP
  def toStringC_(visited: Set[Type]): String = toStringP
}

/**
  A trait describing a value that can be run as a function.
  @author bluebear94
*/
trait FuncLike extends Type {
  /**
    The result of calling this function on zero or more arguments.
  */
  def apply(args: Array[Type]): Type
}
