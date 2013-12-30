package util

import types._

object Indexing {
  def index(t: Type, i: Type): Type = {
    val tt: Int = t.getType
    if (tt == 5 || tt == 6) index(t.asInstanceOf[LList], i.asInstanceOf[TNumerical].intValue)
    else if (tt == 8) index(t.asInstanceOf[LMap], i)
    else if (tt == 3) index(t.asInstanceOf[TString], i.asInstanceOf[TNumerical].intValue)
    else if (tt == 1) index(t.asInstanceOf[TMountain], i.asInstanceOf[TNumerical].intValue)
    else if (tt == 2) index(t.asInstanceOf[THill], i.asInstanceOf[TNumerical].intValue)
    else new TError(1)
  }
  def index(t: LList, i: Int): Type = {
    if (i >= 0 && i < t.l.length) t.l()(i)
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
  def setIndex(t: Type, i: Type, n: Type): Unit = {
    val tt: Int = t.getType
    if (tt == 5 || tt == 6) setIndex(t.asInstanceOf[LList], i.asInstanceOf[TNumerical].intValue, n)
    else if (tt == 8) setIndex(t.asInstanceOf[LMap], i, n)
    else if (tt == 3) setIndex(t.asInstanceOf[TString], i.asInstanceOf[TNumerical].intValue, n)
    else if (tt == 1) setIndex(t.asInstanceOf[TMountain], i.asInstanceOf[TNumerical].intValue, n.toBoolean)
    else if (tt == 2) setIndex(t.asInstanceOf[THill], i.asInstanceOf[TNumerical].intValue, n.toBoolean)
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
}