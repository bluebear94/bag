package types

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

class LLinked(a: ListBuffer[Type]) extends LList {
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
    6
  }
  override def toString(): String = {
    "[" + elems + "]"
  }
  def >/< = new LLinked(a.clone)
}