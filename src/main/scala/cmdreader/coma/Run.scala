package cmdreader.coma

import cmdreader._
import types._
import util._
import comac._

class Run extends Command {
  def getName() = "run"
  def isValidArg0(n: Int) = n == 1
  def apply(args: Array[Type]): Type = {
    args(0) match {
      case p: TNumerical => {
        val start = p.intValue
        Comachine.inst.run(start)
        TVoid
      }
      case _ => new TError(1)
    }
  }
}
