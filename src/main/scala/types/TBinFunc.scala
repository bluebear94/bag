package types

import run.RunningInstance
import cmdreader.Global
import parse.ast.BFuncs

class TBinFunc(bytecode: Array[Byte], source: String, ci: RunningInstance=null, name: String = "[ANON]") extends TFunction {
  def apply(args: Array[Type]): Type = {
    val newci = new RunningInstance(name, Global.top, args)
    Global.top = newci
    newci.bytecode = this.bytecode
    newci.run
    val res = newci.answer
    Global.top = Global.top.calling
    res
  }
  override def toBytecode(): Array[Byte] = bytecode
  override def equals(that: Any): Boolean = {
    that match {
      case other: TBinFunc => other.toBytecode() == bytecode
      case _ => false
    }
  }
  override def hashCode = bytecode.hashCode
  override def toString(): String = s"Func(${BFuncs.bytecodeToString(bytecode)})"
  def >/< = new TBinFunc(bytecode.clone, new String(source), ci)
}