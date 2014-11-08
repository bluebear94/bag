package cmdreader.std
import cmdreader.Command
import types._

class Delay extends Command {
  def getName(): String = "delay"
  def isValidArg0(n: Int): Boolean = n <= 1
  def apply(args: Array[Type]): Type = {
    val ms = if (args.length == 0) 1000 else args(0) match {
      case n: TNumerical => n.intValue
      case x => return new TError(1, s"Cannot pass in $x")
    }
    Thread.sleep(ms)
    TVoid
  }
}
