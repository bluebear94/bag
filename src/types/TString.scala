package types

import util.MakeByteArrays

case class TString(s2: String) extends Type {
  var s = s2
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
  def si(i: Int, c: Char) = {
    s = s.substring(0, i - 1) + new String(Array(c)) + s.substring(i + 1)
  }
  def >/< = new TString(new String(s))
  def toBytecode: Array[Byte] = {
    val bs = s.getBytes
    val bsl = bs.length
    MakeByteArrays.intToByteArray(bsl) ++ bs
  }
}