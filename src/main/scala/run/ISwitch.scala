package run

class ISwitch {
  private var _isSet = false
  def isSet = _isSet
  def trigger() = setTo(true)
  def reset() = setTo(false)
  def setTo(b: Boolean) = _isSet = b
}
