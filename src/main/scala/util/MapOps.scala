package util

import types._
import scala.collection.mutable.HashMap

object MapOps {
  def fromList(l: List[Type]) = {
    def pairArgs[T](args: List[T], h: HashMap[T, T]): Unit = {
      args match {
        case a :: b :: c => {
          h += ((a, b))
          pairArgs(c, h)
        }
        case Nil => ()
        case _ => throw new RuntimeException("Cannot make a map out of an odd number of elements")
      }
    }
    val emptyHash = HashMap.empty[Type, Type]
    pairArgs(l.reverse, emptyHash)
    new LMap(emptyHash)
  }
}