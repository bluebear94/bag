package util

object MakeByteArrays {
  def intToByteArray(bsl: Int): Array[Byte] = {
    val l = new Array[Byte](4)
    for (i <- 0 until 4) {
      l(3 - i) = ((bsl & (0xFF << (i << 3))) >> (i << 3)).toByte
    }
    l
  }
  def longToByteArray(bsl: Long): Array[Byte] = {
    val l = new Array[Byte](8)
    for (i <- 0 until 8) {
      l(7 - i) = ((bsl & (0xFFL << (i << 3))) >> (i << 3)).toByte
    }
    l
  }
}