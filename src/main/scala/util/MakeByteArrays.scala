package util

object MakeByteArrays {
  def intToByteArray(bsl: Int): Array[Byte] = {
    val l = new Array[Byte](4)
    for (i <- 0 to 4) {
      l(4 - i) = (bsl & (0xFF << 8 * i) >> 8 * i).toByte
    }
    l
  }
}