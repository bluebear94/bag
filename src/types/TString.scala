package types

case class TString(s: String) extends Type {
  def getVal(): String = {
    s
  }
  def getType(): Int = {
    3
  }
  def toBoolean(): Boolean = {
    s == ""
  }
  override def toString(): String = {
    "\"" + s + "\""
  }
  def equals(that: Type): Boolean = {
    that.getType() == 3 &&
    that.asInstanceOf[TString].getVal() == s
  }
}