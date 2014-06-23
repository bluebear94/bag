package cmdreader.std

import cmdreader.Command
import types._
import scala.collection.mutable._

class KeyValue extends Command {
  override def getName(): String = "kv"
  override def isValidArg0(n: Int): Boolean = n == 1 || n == 2
  override def apply(args: Array[Type]): Type = {
    args(0) match {
      case m: LMap => {
        val l = m.gm.toList.map { case (k, v) => new LArray(ArrayBuffer(k, v)) }
        if (args.length == 2 && args(1).toBoolean) new LLinked(l.toBuffer.to[ListBuffer])
        else new LArray(l.toBuffer.to[ArrayBuffer])
      }
      case _ => new TError(1, "#1 must be map")
    }
  }
}
