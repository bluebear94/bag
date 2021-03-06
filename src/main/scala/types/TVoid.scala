package types

import scala.collection.mutable._

/**
 * A void value.
 */
class TVoid extends Atom {

  def canEqual(that: Any): Boolean = {
    that.isInstanceOf[TVoid]
  }
  override def equals(that: Any): Boolean = {
    that match {
      case other: TVoid => true
      case _ => false
    }
  }
  def toStringP: String = "Void"
  override def hashCode = 0
  def equalsStrictly(that: Type): Boolean = that.getType == 0
  def getType(): Int = {
    0
  }
  def toBoolean(): Boolean = {
    false
  }
  def toBytecode: Array[Byte] = {
    Array()
  }
  def >/< = new TVoid
  def cast(i: Int): Type = i match {
    case 0 => this
    case 1 => TMountain(0)
    case 2 => THill(0)
    case 3 => TString("")
    case 4 => TFish(0.0)
    case 5 => new LArray(new ArrayBuffer)
    case 6 => new LLinked(new ListBuffer)
    case 7 => new TBinFunc(Array(-0x1f, 0x00, -0x17, 0x53))
    case 8 => new LMap(HashMap())
    case 9 => new TByteString(Array[Byte]())
  }
}
