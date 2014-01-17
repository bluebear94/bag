package cmdreader.std

import cmdreader.Command
import types._
import run._

class Disassembly extends Command {
  override def getName(): String = "dsa"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    args(0) match {
      case f: TBinFunc => new TString(Disassembler.disassemble(f.toBytecode))
      case _ => new TError(1)
    }
  }
}