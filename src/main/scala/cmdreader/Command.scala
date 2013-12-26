package cmdreader
import types._

abstract class Command {
  def getName(): String = ""
  def isValidArg0(n: Int): Boolean = true
  def apply(args: Array[Type]): Type = new TVoid()
}