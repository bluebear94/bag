package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class UnKeyValue extends Command {
  override def getName(): String = "unkv"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val err = new TError(1, "#1 must be a list of lists of at least two elements")
    args(0) match {
      case ls: LList => {
        val map = ls.l.map { _ match {
            case l: LList => {
              val ls = l.l
              if (ls.length < 2) return err
              (ls(0), ls(1))
            }
            case _ => return err
          }
        }.toMap
        new LMap(HashMap(map.toSeq: _*))
      }
      case _ => err
    }
  }
}
