package types

import scala.collection.immutable.Set
import scala.collection.mutable._
import util.{MakeByteArrays, MapOps}

/**
 * A type to define lists of types.
 */
abstract class LList extends Type {
  /**
   * Returns the Buffer containing the list.
   */
  def l: Buffer[Type]
  /**
   * Sets an element.
   * @param index the index at which to set a new element
   * @param n the new element
   */
  def lu(index: Int, n: Type)
  def app(n: Type)
  def equals(that: Type): Boolean = {
    that.isInstanceOf[LList] &&
    that.asInstanceOf[LList].l.equals(this)
  }
  override def hashCode: Int = {
    l.foldLeft(67)(_ * 41 + _.hashCode * 89)
  }
  /**
   * An internal function to list the elements of the list, separated by commas but without braces or brackets.
   */
  def elems(): String = {
    l.mkString(", ")
  }
  def elems(visited: Set[Type]) = {
    l.map((e: Type) => e.toStringC(visited + this)).mkString(", ")
  }
  def toBoolean(): Boolean = {
    !l.isEmpty
  }
  def toBytecode: Array[Byte] = {
    val s = l.map((t: Type) => {
      val bc = t.toBytecode
      MakeByteArrays.intToByteArray(bc.length) ++ Array[Byte](t.getType.toByte) ++ bc
    }).foldLeft(Array[Byte]())(_ ++ _)
    val bsl = l.length
    MakeByteArrays.intToByteArray(bsl) ++ s
  }
  override def equals(that: Any) = {
    that match {
      case other: LList => l.equals(other.l)
      case _ => false
    }
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: LList if (getType == that.getType) => l == t.l
      case _ => false
    }
  }
  def cast(i: Int) = i match {
    case 5 => new LArray(l.to[ArrayBuffer])
    case 6 => new LLinked(l.to[ListBuffer])
    case 0 => new TVoid
    case 8 => MapOps.fromList(l.toList)
  }
}
