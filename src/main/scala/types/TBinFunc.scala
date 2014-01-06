package types

import run.RunningInstance
import cmdreader.Global

class TBinFunc(bytecode: Array[Byte], source: String, ci: RunningInstance) extends TFunction {
  def apply(args: Array[Type]): Type = {
    val newci = new RunningInstance("code: fcall", Global.top, args)
    Global.top = newci
    newci.bytecode = this.bytecode
    newci.run
    val res = newci.answer
    Global.top = Global.top.calling
    res
  }
  def getBytecode(): Array[Byte] = bytecode
  override def equals(that: Any): Boolean = {
    that match {
      case other: TBinFunc => other.getBytecode() == bytecode
      case _ => false
    }
  }
  //override def toString(): String = "λ\n" + source + "\nEndλ"
  def >/< = new TBinFunc(bytecode.clone, new String(source), ci)
}