package parse.ast

/**
 * Utilities for applying changes to Amethyst code for it to be parsed.
 * @author bluebear94
 */
object Preprocessor {
  def preprocessLn(line: String) = {
    var quoteMode = false
    var bs = false
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
        case '\"' => if (!bs) {
          quoteMode = !quoteMode
        }
        case '(' | '[' | '{' | '«' => if (!quoteMode) st += c
        case ')' => if (!quoteMode) pop('(')
        case ']' => if (!quoteMode) pop('[')
        case '}' => if (!quoteMode) pop('{')
        case '»' => if (!quoteMode) pop('«')
        case '_' => if (!quoteMode) {
          i -= 1
          cont = false
        }
        case _ => ()
      }
      i += 1
      bs = c == '\\'
    }
    var sub = line.substring(0, i) + (if (quoteMode) "\"" else "")
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
  def minIndex(si: Int, ni: Int) = {
    (si, ni) match {
      case (-1, -1) => -1
      case (-1, _) => ni
      case (_, -1) => si
      case (_, _) => Math.min(si, ni)
    }
  }
  def preprocess(c: String, debug: Boolean = false): String = {
    // You are not expected to understand what the f*ck this means.
    val code = if (c.last == '\n') c else c + "\n"
    var qm = false
    var out = ""
    var curLine = ""
    var l = 0
    var s = 0
    if (debug) {
      println("Preprocessing: ")
      println(code.replaceAll("\n", ";") + "|")
    }
    while (l < code.length) {
      val c = code.charAt(l)
      if (debug) {
        for (i: Int <- 0 until s) {
          print(" ")
        }
        for (i: Int <- s until l) {
          print("-")
        }
        println("> " + (out + curLine).replaceAll("\n", ";"))
      }
      if (c == '\"') qm = !qm
      else if (c == '\n' || (!qm && c == ';')) {
        out += preprocessLn(curLine + code.substring(s, l))
        s = l
      } else if (c == 'λ' && !qm) {
        var lvs = 1
        var index = l + 1
        while (lvs != 0) {
          val li = code.indexOf("λ", index)
          val ei = code.indexOf("Endλ", index)
          val newIndex = minIndex(li, ei)
          if (newIndex != -1 && debug) {
            for (i: Int <- 0 until newIndex) {
              print(" ")
            }
            println(".")
          }
          if (newIndex == -1) throw new RuntimeException("unmatched lambda constructs (missing Endλ)")
          else if (newIndex == li) {
            index = li + 1;
            lvs += 1;
          } else if (newIndex == ei) {
            index = Math.min(code.length, ei + 4);
            lvs -= 1;
            if (lvs < 0) throw new RuntimeException("unmatched lambda constructs (extra Endλ)")
          } else throw new RuntimeException("I just don't know what went wrong :(")
        }
        // post: index holds one more than the index of the last applicable closing statement
        val left = code.substring(s, l + 1)
        val right = code.substring(index)
        val mid = code.substring(l + 1, index - 1)
        curLine += left + preprocess(mid) + "λ"
        s = index
        l = s - 1
        if (debug) {
          println("Preprocessing again: ")
          println(code.replaceAll("\n", ";") + "|")
        }
      }
      l += 1
    }
    if (debug) println(out)
    out
    //out.replaceAll("\n","\n;") // with your memory-wasting habits!
  }
}