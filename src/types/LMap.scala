package types

import scala.collection.mutable.HashMap

class LMap(h: HashMap[Type, Type]) extends Type {
  def getType(): Int = 8
  def gm() = h
  def toBoolean(): Boolean = h.isEmpty
  def equals(that: Type) = {
    (that.getType == 8) && h == that.asInstanceOf[LMap].gm 
  }
  override def toString(): String = {
    "«" + h.toList.map(p => (p._1.toString + " → " + p._2.toString)).mkString(",") + "»"
  }
  def lu(i: Type, n: Type) = {
    h(i) = n
  }
  def >/< = new LMap(h.clone)
}