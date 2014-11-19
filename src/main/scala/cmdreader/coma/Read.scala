package cmdreader.coma

import cmdreader._
import types._
import util._
import comac._

class Read extends Command {
  def getName() = "read"
  def isValidArg0(n: Int) = n == 2
  def apply(args: Array[Type]): Type = {
    (args(0), args(1)) match {
      case (p: TNumerical, q: TNumerical) => {
        val start = p.intValue
        val end = q.intValue
        TByteString(Comachine.inst.mem.slice(start, end))
      }
      case (_, _) => new TError(1)
    }
  }
}
