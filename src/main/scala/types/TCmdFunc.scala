package types

import cmdreader.Global

class TCmdFunc(name: String) extends TFunction {
  def apply(args: Array[Type]): Type = Global.getCmdno(name)(args)
  override def equals(that: Any): Boolean = {
    that match {
      case other: TCmdFunc => name == other.toString
      case _ => false
    }
  }
  override def hashCode = name.hashCode
  override def toString(): String = name
  def >/< = new TCmdFunc(new String(name))
}