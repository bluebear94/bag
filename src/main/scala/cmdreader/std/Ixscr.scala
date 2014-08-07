
package cmdreader.std

import cmdreader._
import types._
import util._
import run._

class Ixscr extends Command {
  def getName = "ixscr"
  def isValidArg0(n: Int) = n == 3 || n == 4
  def apply(args: Array[Type]): Type = {
    try {
      val query = CollectionOps.decodeToList(args(0))
      val replacement = CollectionOps.decodeToList(args(1))
      CollectionOps.ctc(l => {
        CollectionOps.replaceFirstOccurrence(query, replacement, l) match {
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
