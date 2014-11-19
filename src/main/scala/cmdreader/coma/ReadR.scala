package cmdreader.coma

import cmdreader._
import types._
import util._
import comac._

class ReadR extends Command {
  def getName() = "readr"
  def isValidArg0(n: Int) = n == 1
  def apply(args: Array[Type]): Type = {
    args(0) match {
      case p: TNumerical => {
        val rg = p.intValue
        THill(Comachine.inst.reg(rg))
      }
      case _ => new TError(1)
    }
  }
}
