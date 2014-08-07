
package cmdreader.std

import cmdreader._
import types._
import util._
import run._

class Idxre extends Command {
  def getName = "idxre"
  def isValidArg0(n: Int) = n == 3
  def apply(args: Array[Type]): Type = {
    CollectionOps.ctc(CollectionOps.replaceAllOccurrences(args(0), args(1), _))(args(2))
  }
}
