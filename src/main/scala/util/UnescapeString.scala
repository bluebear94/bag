package util

/**
  Yet another random object with a utility function.
  @author bluebear94
*/
object UnescapeString {
  private def isOctal(c: Char): Boolean = '0' <= c && c < '8'
  private def isHex(c: Char): Boolean = '0' <= c && c <= '9' || 'A' <= c && c <= 'F'  || 'a' <= c && c <= 'f'
  private def toHexValue(c: Char): Int = {
    if ('0' <= c && c <= '9') c - '0'
    else if ('A' <= c && c <= 'F') c - 'A' + 10
    else if ('a' <= c && c <= 'f') c - 'a' + 10
    else -1
  }
  private def concatOpt(a: String, b: Option[String]): Option[String] = {
    b match {
      case Some(d) => Some(a + d)
      case None => None
    }
  }
  private def cts(c: Char): String = new String(Array(c))
  /**
    Unescapes a string; i. e. substitutes escape sequences such as \n with their literal counterparts (in this case the newline).
    @param orig the string with escape sequences
    @return a Some value with the result, or None if unsuccessful
  */
  def unescape(orig: String): Option[String] = {
    if (orig == "") Some("")
    else if (orig.charAt(0) == '\\') { // an escape sequence
      val next = orig.charAt(1)
      if (isOctal(next)) { // octal
        if (!(orig.length >= 4 && isOctal(orig.charAt(2)) && isOctal(orig.charAt(3)))) None
        else {
          val octalValue: Char = ((next - '0') << 6 + (orig.charAt(2) - '0') << 3 + (orig.charAt(3) - '0')).toChar
          concatOpt(cts(octalValue), unescape(orig.substring(4)))
        }
      }
      else if (next == 'x' || next == 'u') { // hex
        if (!(orig.length >= 4 && isHex(orig.charAt(2)) && isHex(orig.charAt(3)))) None
        else {
          if (orig.length >= 6 && isHex(orig.charAt(4)) && isHex(orig.charAt(5))) { // 4 digits
            var hexValue: Int = 0
            for (i <- 0 until 4) {
              hexValue <<= 4
              hexValue += toHexValue(orig.charAt(2 + i))
            }
            concatOpt(cts(hexValue.toChar), unescape(orig.substring(6)))
          }
          else { // 2 digits
            val hexValue = toHexValue(orig.charAt(2)) << 4 + toHexValue(orig.charAt(3))
            concatOpt(cts(hexValue.toChar), unescape(orig.substring(4)))
          }
        }
      }
      else if (next == '\'' || next == '\"' || next == '\\') concatOpt(cts(next), unescape(orig.substring(2)))
      else if (next == 'b') concatOpt(cts('\b'), unescape(orig.substring(2)))
      else if (next == 'f') concatOpt(cts('\f'), unescape(orig.substring(2)))
      else if (next == 'n') concatOpt(cts('\n'), unescape(orig.substring(2)))
      else if (next == 'r') concatOpt(cts('\r'), unescape(orig.substring(2)))
      else if (next == 't') concatOpt(cts('\t'), unescape(orig.substring(2)))
      else None
    }
    else if (orig.charAt(0) == '\"') {
      None
    }
    else {
      concatOpt(orig.substring(0, 1), unescape(orig.substring(1)))
    }
  }
}
