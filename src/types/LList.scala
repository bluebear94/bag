package types

import scala.collection.mutable._
import util.MakeByteArrays

abstract class LList extends Type {
  def l(): Buffer[Type]
  def lu(index: Int, n: Type)
  def equals(that: Type): Boolean = {
    that.isInstanceOf[LList] &&
    that.asInstanceOf[LList].l().equals(this)
  }
  def elems(): String = {
    l().mkString(", ")
  }
  def toBoolean(): Boolean = {
    l().isEmpty
  }
  def toBytecode: Array[Byte] = {
    val s = l.map((t: Type) => {
      val bc = t.toBytecode
      MakeByteArrays.intToByteArray(bc.length) ++ bc
    }).foldLeft(Array[Byte]())(_ ++ _)
    val bsl = l.length
    MakeByteArrays.intToByteArray(bsl) ++ s
  }
}