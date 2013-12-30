package types

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

class LArray(a: ArrayBuffer[Type]) extends LList {
  def l(): Buffer[Type] = {
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
  override def toString(): String = {
    "{" + elems + "}"
  }
  def >/< = new LArray(a.clone)
}