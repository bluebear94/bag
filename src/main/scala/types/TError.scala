package types

class TError(errno: Int, msg: String) extends Atom {
  def this(errno: Int) = this(errno, "")
  def getErrno(): Int = errno
  def getType(): Int = -1
  def toBoolean(): Boolean = false
  override def equals(that: Any): Boolean = {
    that match {
      case other: TError => errno == other.getErrno()
      case _ => false
    }
  }
  override def hashCode = errno
  def equalsStrictly(that: Type) = {
    throw new UnsupportedOperationException
  }
  def toStringP(): String = {
    "Error #" + errno + ": " + msg
  }
  def >/< = new TError(errno, msg)
  def toBytecode: Array[Byte] = Array[Byte]()
  def cast(i: Int): Type = throw new UnsupportedOperationException("cannot cast a TError")
}
