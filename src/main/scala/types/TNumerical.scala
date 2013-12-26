package types

abstract class TNumerical extends Type {
  def getVal(): Any
  def toBoolean(): Boolean = {
    getVal() == 0
  }
  def intValue(): Int
}