package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class Sub extends Command {
  override def getName(): String = "sub"
  override def isValidArg0(n: Int): Boolean = n == 2 || n == 3
  override def apply(args: Array[Type]): Type = {
    val whole = args(0)
    (whole, args(1), if (args.length == 2) 0 else args(2)) match {
      case (w: LList, n: TNumerical, p: TNumerical) => {
        val const = (b: Buffer[Type]) => {
          if (w.getType == 5) new LArray(b.to[ArrayBuffer])
          else new LLinked(b.to[ListBuffer])
        }
        const({
          if (args.length == 2) w.l.drop(n.intValue)
          else w.l.slice(n.intValue, p.intValue)
        })
      }
      case (w: TString, n: TNumerical, p: TNumerical) => {
        new TString({
          if (args.length == 2) w.getVal.substring(n.intValue)
          else w.getVal.substring(n.intValue, p.intValue)
        })
      }
      case (dick, tom, harry) => new TError(1, s"($dick, $tom, $harry) are not valid arguments")
    }
      
  }
}
