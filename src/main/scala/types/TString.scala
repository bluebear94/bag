package types

import util.MakeByteArrays
import parse.ast._
import gui.Main

/**
 * A type to define UTF-8 strings.
 * @author bluebear94
 */
case class TString(s2: String) extends Type {
  var s = s2
  def getVal(): String = {
    s
  }
  def getType(): Int = {
    3
  }
  def toBoolean(): Boolean = {
    s != ""
  }
  override def toString(): String = {
    s
  }
  def equals(that: Type): Boolean = {
    that.getType() == 3 &&
      that.asInstanceOf[TString].getVal() == s
  }
  override def hashCode = s.hashCode
  def equalsStrictly(that: Type) = {
    that match {
      case t: TString => s == t.s
      case _ => false
    }
  }
  def si(i: Int, c: Char) = {
    if (i == 0) s = new String(Array(c)) + s.substring(1)
    else if (i == s.length - 1) s = s.substring(0, i) + new String(Array(c))
    else s = s.substring(0, i - 1) + new String(Array(c)) + s.substring(i + 1)
  }
  def >/< = new TString(new String(s))
  def toBytecode: Array[Byte] = {
    s.getBytes("UTF-8")
  }
  def cast(i: Int): Type = {
    try {
      val bytes = WholeParser.parse(s, Main.p)
      val bf = new TBinFunc(bytes, s, null, "expr - " + s)
      val x = bf(Array())
      if (i == x.getType) x else new TError(1)
    }
    catch {
      case e: Exception => new TError(1)
    }
  }
}