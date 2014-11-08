package cmdreader.std

import cmdreader.Command
import types._
import run._
import rwvar.VariableReader

class Disassembly extends Command {
  override def getName(): String = "dsa"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    args(0) match {
      case f: TBinFunc => {
        val bytecode = f.toBytecode
        println(bytecode.mkString(","))
        val (ref, i) = VariableReader.readProtocolPart(bytecode)
        val (vel, j) = VariableReader.readProtocolPart(bytecode, i)
        new TString("Passed by reference: " + ref.mkString(", ") +
          "\nPassed by value: " + vel.mkString(", ") + "\n" +
          Disassembler.disassemble(bytecode drop j))
      }
      case _ => new TError(1)
    }
  }
  override def isPure = true
}
