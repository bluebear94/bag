package types

import parse.ast.BFuncs
import util._
import scala.collection.mutable._
import scala.math.BigInt

case class TByteString(var a: Array[Byte]) extends Atom {
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
      case t: TByteString => a sameElements t.a
      case t: TString => a.equals(t.s.getBytes())
      case t: LList => a.toList.map((b: Byte) => THill(b.toLong)).zip(t.l).forall {
        case (x, y: TNumerical) => x.n == y.getVal}
      case _ => false
    }
  }
  override def hashCode = {
    a.foldLeft(67)(_ * 41 + _ * 89)
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: TByteString => a.equals(t.a)
      case _ => false
    }
  }
  def toStringP: String = s"Bytes(${BFuncs.bytecodeToString(a)})"
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
  def cast(i: Int) = i match {
    case 9 => this
    case 0 => TVoid.inst
    case 7 => new TBinFunc(a)
    case 5 => new LArray(a.map(new TMountain(_).asInstanceOf[Type]).to[ArrayBuffer])
    case 6 => new LLinked(a.map(new TMountain(_).asInstanceOf[Type]).to[ListBuffer])
    case 1 => TMountain(BigInt(a))
  }
}
