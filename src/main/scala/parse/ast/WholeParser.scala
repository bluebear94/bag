package parse.ast

import scala.util.parsing.input._
import scala.util.parsing.combinator._

object WholeParser {
  def parse(code: String, p: XprInt) = {
    def isNL(c: Char) = c == ';' || c == '\n'
    val preprocessedCode = Preprocessor.preprocess(code)
    import p._
    var bytes = Array[Byte]()
    var rest = new p.PackratReader(new CharSequenceReader(preprocessedCode))
    while (rest.first != CharSequenceReader.EofCh) {
      val line = p.expression(rest)
      line match {
        case Success(res, nx) => {
          rest = new p.PackratReader(nx)
          bytes = bytes ++ BFuncs.flatten(res.toBytecode) ++ Array[Byte](-0x17, 0x53)
        }
        case NoSuccess(msg, _) => throw new RuntimeException(msg)
      }
      //if (!isNL(rest.first) && rest.first != CharSequenceReader.EofCh)
      //  throw new RuntimeException("Lines must be delimited")
      while (isNL(rest.first)) rest = new p.PackratReader(rest.rest)
    }
    bytes
  }
}