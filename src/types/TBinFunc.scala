package types

import run.RunningInstance

class TBinFunc(bytecode: Array[Byte], source: String, ci: RunningInstance) extends TFunction {
  def apply(args: Array[Type]): Type = new TVoid // TODO, yet another stub
  def getBytecode(): Array[Byte] = bytecode
  override def equals(that: Type): Boolean = that.isInstanceOf[TBinFunc] && that.asInstanceOf[TBinFunc].getBytecode() == bytecode
  override def toString(): String = "λ\n" + source + "\nEndλ"
}