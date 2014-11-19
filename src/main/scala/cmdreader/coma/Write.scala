package cmdreader.coma

import cmdreader._
import types._
import util._
import comac._

class Write extends Command {
  def getName() = "write"
  def isValidArg0(n: Int) = n == 2
  def apply(args: Array[Type]): Type = {
    (args(0), args(1)) match {
      case (TByteString(s), p: TNumerical) => {
        val start = p.intValue
        s.copyToArray(Comachine.inst.mem)
      }
      case (_, _) => return new TError(1)
    }
    TVoid.inst
  }
}
