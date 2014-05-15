package util

import types._

object Indexing {
  def index(t: Type, i: Type): Type = {
    t match {
      case t: LList => index(t, i.asInstanceOf[TNumerical].intValue)
      case t: LMap => index(t, i)
      case t: TString => index(t, i.asInstanceOf[TNumerical].intValue)
      case t: TMountain => index(t, i.asInstanceOf[TNumerical].intValue)
      case t: THill => index(t, i.asInstanceOf[TNumerical].intValue)
      case t: TByteString => index(t, i.asInstanceOf[TNumerical].intValue)
      case _ => new TError(1)
    }
  }
  def index(t: LList, i: Int): Type = {
    if (i >= 0 && i < t.l.length) t.l(i)
    else new TError(5)
  }
  def index(t: LMap, i: Type): Type = {
    t.gm()(i)
  }
  def index(t: TString, i: Int): Type = {
    THill(t.getVal().charAt(i))
  }
  def index(t: TMountain, i: Int): Type = {
    THill(if (t.getVal.testBit(i)) 1l else 0l)
  }
  def index(t: THill, i: Int): Type = {
    THill(if ((t.getVal & (1 << i)) == 0) 0l else 1l)
  }
  def index(t: TByteString, i: Int): Type = {
    THill(t.a(i))
  }
  def setIndex(t: Type, i: Type, n: Type): Unit = {
    t match {
      case t: LList => setIndex(t, i.asInstanceOf[TNumerical].intValue, n)
      case t: LMap => setIndex(t, i, n)
      case t: TString => setIndex(t, i.asInstanceOf[TNumerical].intValue, n)
      case t: TMountain => setIndex(t, i.asInstanceOf[TNumerical].intValue, n.toBoolean)
      case t: THill => setIndex(t, i.asInstanceOf[TNumerical].intValue, n.toBoolean)
      case t: TByteString => setIndex(t, i.asInstanceOf[TNumerical].intValue, n.asInstanceOf[TNumerical].intValue.toByte)
      case _ => new TError(1)
    }
  }
  def setIndex(t: LList, i: Int, n: Type): Unit = {
    if (i >= 0 && i < t.l.length) t.lu(i, n)
    else if (i == t.l.length) t.app(n)
    else new TError(5)
  }
  def setIndex(t: LMap, i: Type, n: Type): Unit = {
    t.lu(i, n)
  }
  def setIndex(t: TString, i: Int, n: Type): Unit = {
    t.si(i, n.asInstanceOf[TNumerical].intValue.toChar)
  }
  def setIndex(t: TMountain, i: Int, n: Boolean): Unit = {
    t.si(i, n)
  }
  def setIndex(t: THill, i: Int, n: Boolean): Unit = {
    t.snv({
      if (n) t.getVal | (1 << i)
      else t.getVal & ~(1 << i)
    })
  }
  def setIndex(t: TByteString, i: Int, n: Byte) = {
    t.a(i) = n
  }
  def delIndex(t: Type, i: Type): Unit = {
    t match {
      case tt: LMap => delIndex(tt, i): Unit
    }
  }
  def delIndex(t: LMap, i: Type): Unit = {
    t.du(i)
  }
}