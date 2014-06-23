package types

import cmdreader.Global
import scala.collection.mutable.HashMap

class TCmdFunc(name: String) extends TFunction with Atom {
  override def apply(args: Array[Type]): Type = Global.getCmdno(name)(args)
  def applyWith(args: Array[Type], closure: HashMap[String, Type]) = new TError(1)
  override def equals(that: Any): Boolean = {
    that match {
      case other: TCmdFunc => name == other.toString
      case _ => false
    }
  }
  override def hashCode = name.hashCode
  def toStringP: String = name
  def >/< = new TCmdFunc(new String(name))
}
