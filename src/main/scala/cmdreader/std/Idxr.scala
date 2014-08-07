
package cmdreader.std

import cmdreader._
import types._
import util._
import run._

class Idxr extends Command {
  def getName = "idxr"
  def isValidArg0(n: Int) = n == 3 || n == 4
  def apply(args: Array[Type]): Type = {
    try {
      CollectionOps.ctc(l => {
        CollectionOps.replaceFirstOccurrence(args(0), args(1), l) match {
          case Some(nl) => nl
          case None => {
            if (args.length == 4 && args(3).toBoolean) throw new RuntimeException
            else l
          }
        }
      })(args(2))
    } catch {
      case e: RuntimeException => new TError(6, s"${args(0)} not found in ${args(2)}; strict mode on")
    }
  }
}
