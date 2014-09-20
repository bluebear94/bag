package types

import run.RunningInstance
import cmdreader.Global
import parse.ast.BFuncs
import scala.collection.mutable.HashMap
import gui.Main

class TBinFunc(bytecode: Array[Byte], source: String = "", ci: RunningInstance=null, name: String = "[ANON]", _protocol: FProtocol = FProtocol.empty) extends TFunction with Atom {
  def applyWith(args: Array[Type], closure: HashMap[String, Type]): Type = {
    val newci = new RunningInstance(name, Global.top, args)
    Global.top = newci
    newci.bytecode = this.bytecode
    newci.environment = closure
    newci.run(Main.switch)
    val res = newci.answer
    Global.top = Global.top.calling
    res
  }
  def protocol = _protocol
  override def toBytecode(): Array[Byte] = _protocol.toBytecode ++ bytecode
  override def equals(that: Any): Boolean = {
    that match {
      case other: TBinFunc => other.toBytecode() == bytecode
      case _ => false
    }
  }
  override def hashCode = bytecode.hashCode
  def toStringP: String = s"Func(${BFuncs.bytecodeToString(bytecode)})"
  def >/< = new TBinFunc(bytecode.clone, source, ci, name, _protocol)
  def rename(newName: String) = new TBinFunc(bytecode, source, ci, newName, _protocol)
}
