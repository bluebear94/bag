package types

import cmdreader.Global

class TCmdFunc(name: String) extends Type {
  def apply(args: Array[Type]): Type = Global.getCmd(name)(args)
  def equals(that: Type): Boolean = name == that.toString
  def toString(): String = name
}