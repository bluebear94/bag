package types

import scala.collection.mutable.HashMap
import util.MakeByteArrays

class LMap(h: HashMap[Type, Type]) extends Type {
  def getType(): Int = 8
  def gm() = h
  def toBoolean(): Boolean = h.isEmpty
  def equals(that: Type) = {
    (that.getType == 8) && h == that.asInstanceOf[LMap].gm
  }
  override def hashCode: Int = {
    h.foldLeft(83)((a, b) => 97 * a + 41 * b._1.hashCode + 101 * b._2.hashCode)
  }
  override def toString(): String = {
    "Map(" + h.toList.map(p => (p._1.toString + " → " + p._2.toString)).mkString(", ") + ")"
  }
  def lu(i: Type, n: Type) = {
    h(i) = n
  }
  def du(i: Type) = {
    h.remove(i)
  }
  def >/< = new LMap(h.clone)
  def toBytecode: Array[Byte] = {
    val s = h.toList.map((p: (Type, Type)) => {
      val t = p._1
      val u = p._2
      val bc = t.toBytecode
      val cc = u.toBytecode
      MakeByteArrays.intToByteArray(bc.length) ++ Array[Byte](t.getType.toByte) ++ bc ++
        MakeByteArrays.intToByteArray(cc.length) ++ Array[Byte](u.getType.toByte) ++ cc
    }).foldLeft(Array[Byte]())(_ ++ _)
    val bsl = h.toList.length
    MakeByteArrays.intToByteArray(bsl) ++ s
  }
  override def equals(that: Any) = {
    that match {
      case other: LMap => gm.equals(other.gm)
      case _ => false
    }
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: LMap => h == t.gm
      case _ => false
    }
  }
}