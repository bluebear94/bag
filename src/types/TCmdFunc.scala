package types

import cmdreader.Global

class TCmdFunc(name: String) extends TFunction {
  def apply(args: Array[Type]): Type = Global.getCmd(name)(args)
  override def equals(that: Type): Boolean = name == that.toString
  override def toString(): String = name
}