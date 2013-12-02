package types

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

class LLinked(a: ListBuffer[Type]) extends LList {
  def l(): Buffer[Type] = {
    a
  }
  def getType(): Int = {
    6
  }
  override def toString(): String = {
    "[" + elems + "]"
  }
}