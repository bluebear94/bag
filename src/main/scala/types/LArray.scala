package types

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

class LArray(a: ArrayBuffer[Type]) extends LList {
  def l: Buffer[Type] = {
    a
  }
  def lu(i: Int, n: Type) {
    a(i) = n
  }
  def app(n: Type) {
    a += n
  }
  def getType(): Int = {
    5
  }
  def toStringNC(): String = "{" + elems + "}"
  def toStringC_(visited: Set[Type]): String = "{" + elems(visited) + "}"
  def >/< = new LArray(a.map(_.>/<))
}
