package parse.ast

object Preprocessor {
  def preprocessLn(line: String) = {
    var quoteMode = false
    var st = ""
    def pop(c: Char) = {
      val l = st.length - 1
      if (st.charAt(l) == c) st = st.substring(0, l)
      else throw new RuntimeException("delimiters not matching")
    }
    var i = 0
    var cont = true
    while (i < line.length && cont) {
      val c = line.charAt(i)
      c match {
        case '\"' => quoteMode = !quoteMode
        case '(' | '[' | '{' | '«' => st += c
        case ')' => pop('(')
        case ']' => pop('[')
        case '}' => pop('{')
        case '»' => pop('«')
        case '_' => if (!quoteMode) {
          i -= 1
          cont = false
        }
        case _ => ()
      }
      i += 1
    }
    var sub = line.substring(0, i)
    for (c <- st.reverse) {
      c match {
        case '(' => sub += ')'
        case '[' => sub += ']'
        case '{' => sub += '}'
        case '«' => sub += '»'
      }
    }
    sub
  }
}