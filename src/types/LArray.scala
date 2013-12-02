package types

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

class LArray(a: ArrayBuffer[Type]) extends LList {
  def l(): Buffer[Type] = {
    a
  }
  def getType(): Int = {
    5
  }
  override def toString(): String = {
    "{" + elems + "}"
  }
}