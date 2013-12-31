package util

object MakeByteArrays {
  def intToByteArray(bsl: Int): Array[Byte] = {
    val l = new Array[Byte](4)
    for (i <- 0 until 4) {
      l(3 - i) = ((bsl & (0xFF << (i << 3))) >> (i << 3)).toByte
    }
    l
  }
}