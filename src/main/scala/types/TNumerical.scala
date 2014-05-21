package types

import scala.math.BigInt
import scala.collection.mutable._

/**
 * A class to define numbers.
 * @author bluebear94
 */
abstract class TNumerical extends Type {
  /**
   * Gets the value of this type.
   */
  def getVal(): Any
  def toBoolean(): Boolean = {
    getVal() == 0
  }
  /**
   * Gets the 32-bit integer value of this type.
   */
  def intValue(): Int
  /**
   * Gets the 64-bit floating-point value of this type.
   */
  def doubleValue(): Double
  def equalsT(that: Type): Boolean
  override def equals(that: Any): Boolean = {
    that match {
      case other: TNumerical => equalsT(other)
      case _ => false
    }
  }
  def cast(i: Int): Type = {
    if (i == getType) this
    else i match {
      case 0 => new TVoid
      case 1 => TMountain(getVal match {
        case a: Long => a
        case b: Double => b.toLong
      })
      case 2 => THill(getVal match {
        case a: BigInt => a.longValue
        case b: Double => b.toLong
      })
      case 3 => TString(toString)
      case 4 => TFish(getVal match {
        case a: BigInt => a.doubleValue
        case b: Long => b
      })
      case 5 => new LArray(ArrayBuffer(this))
      case 6 => new LLinked(ListBuffer(this))
      case 8 => new LMap(HashMap((this, new TVoid)))
      case 9 => new TByteString(toBytecode)
    }
  }
}