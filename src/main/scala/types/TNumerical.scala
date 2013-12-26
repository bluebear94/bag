package types

abstract class TNumerical extends Type {
  def getVal(): Any
  def toBoolean(): Boolean = {
    getVal() == 0
  }
  def intValue(): Int
  def equalsT(that: Type): Boolean
  override def equals(that: Any): Boolean = {
    that match {
      case other: TNumerical => equalsT(other)
      case _ => false
    }
  }
}