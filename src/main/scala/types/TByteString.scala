package types

import parse.ast.BFuncs
import util._

case class TByteString(aa: Array[Byte]) extends Type {
  var a = aa
  /**
   * Returns the type identifier of the instance.
   * @return the type ID
   */
  def getType(): Int = 9
  /**
   * Returns the implied Boolean value of the instance.
   * @return false if 0, ↼0, "", 0.0, {}, or []; true otherwise
   */
  def toBoolean(): Boolean = !a.isEmpty
  override def equals(that: Any): Boolean = {
    that match {
      case t: TByteString => a.equals(t.a)
      case t: TString => a.equals(t.s.getBytes())
      case t: LList => !(a.toList.map((b: Byte) => THill(b.toLong)).zip(t.l).map{
        case (x, y: TNumerical) => x.n == y.getVal}.contains(false))
      case _ => false
    }
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: TByteString => a.equals(t.a)
      case _ => false
    }
  }
  override def toString(): String = s"Bytes(${BFuncs.bytecodeToString(a)})"
  /**
   * Returns a clone of this type.
   */
  def >/<(): Type = TByteString(a.clone)
  /**
   * Returns the binary representation of this type.
   */
  def toBytecode: Array[Byte] = {
    val bsl = a.length
    MakeByteArrays.intToByteArray(bsl) ++ a
  }
}