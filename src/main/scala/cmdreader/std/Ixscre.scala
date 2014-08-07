
package cmdreader.std

import cmdreader._
import types._
import util._
import run._

class Ixscre extends Command {
  def getName = "ixscre"
  def isValidArg0(n: Int) = n == 3
  def apply(args: Array[Type]): Type = {
    val query = CollectionOps.decodeToList(args(0))
    val replacement = CollectionOps.decodeToList(args(1))
    CollectionOps.ctc(CollectionOps.replaceAllOccurrences(query, replacement, _))(args(2))
  }
}
