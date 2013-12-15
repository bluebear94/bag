package types

import scala.collection.mutable._

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
}