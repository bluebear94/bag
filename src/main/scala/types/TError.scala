package types

class TError(errno: Int) extends Type {
  def getErrno(): Int = errno
  def getType(): Int = -1
  def toBoolean(): Boolean = false
  override def equals(that: Any): Boolean = {
    that match {
      case other: TError => errno == other.getErrno()
      case _ => false
    }
  }
  override def toString(): String = {
    "Error #" + errno
  }
  def >/< = new TError(errno)
  def toBytecode: Array[Byte] = Array[Byte]()
}