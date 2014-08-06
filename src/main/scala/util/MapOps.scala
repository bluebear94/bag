package util

import types._
import scala.collection.mutable.HashMap

/**
  Collection of map operations.
  @author bluebear94
*/
object MapOps {
  /**
    Constructs a map from a list.
    @param l a list of entries alternating between key and value
  */
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
