package types

class TError(errno: Int) extends Type {
  def getErrno(): Int = errno
  def getType(): Int = -1
  def toBoolean(): Boolean = false
  def equals(that: Type): Boolean = {
    that.getType() == -1 &&
    that.asInstanceOf[TError].getErrno() == errno
  }
  override def toString(): String = {
    "Error #" + errno
  }
  def >/< = new TError(errno)
}