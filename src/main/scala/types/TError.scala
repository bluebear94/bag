package types

class TError(errno: Int, msg: String) extends Type {
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
  def equalsStrictly(that: Type) = {
    throw new UnsupportedOperationException
  }
  override def toString(): String = {
    "Error #" + errno + ": " + msg
  }
  def >/< = new TError(errno, msg)
  def toBytecode: Array[Byte] = Array[Byte]()
}