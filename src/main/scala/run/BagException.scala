package run

import types._

class BagException(e: TError, pos: Int, fname: String) extends RuntimeException {
  override def getMessage: String = s"${e.toString}: $pos @ $fname"
}
