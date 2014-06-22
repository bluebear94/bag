package types

import scala.collection.mutable.HashMap


/**
 * An abstract representation of a function.
 */
abstract class TFunction extends Type {
  def getType(): Int = {
    7
  }
  def apply(args: Array[Type]): Type = applyWith(args, HashMap.empty)
  def applyWith(args: Array[Type], closure: HashMap[String, Type]): Type
  override def equals(that: Any): Boolean = false
  override def equalsStrictly(that: Any) = equals(that)
  def equalsStrictly(that: Type) = equals(that)
  def toBoolean(): Boolean = true
  override def toString(): String = "a function"
    //TODO method stub
  def toBytecode = Array[Byte]()
  def cast(i: Int) = i match {
    case 7 => this
    case 0 => new TVoid
    case 9 => new TByteString(toBytecode)
  }
}