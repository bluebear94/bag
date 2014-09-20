package run

import types._

/**
  An exception corresponding to a Bag error.
*/
class BagException(val e: TError, pos: Int, fname: String) extends RuntimeException {
  override def getMessage: String = s"${e.toString}: $pos @ $fname"
}
