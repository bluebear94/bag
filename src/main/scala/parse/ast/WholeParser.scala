package parse.ast

import scala.util.parsing.input._
import scala.util.parsing.combinator._
import logger.Logger

object WholeParser {
  def parse(code: String, p: XprInt) = {
    def isNL(c: Char) = c == ';' || c == '\n'
    val preprocessedCode = Preprocessor.preprocess(code).replaceAll("\n","\n;")
    Logger.println(preprocessedCode, -1)
    import p._
    var bytes = Array[Bin]()
    var rest = new p.PackratReader(new CharSequenceReader(preprocessedCode))
    var cx = new ParseContext
    while (rest.first != CharSequenceReader.EofCh) {
      val line = p.expression(rest)
      line match {
        case Success(res, nx) => {
          rest = new p.PackratReader(nx)
          bytes = BFuncs.app(bytes, res.toBytecode(cx) ++ Array(Bytes(Array[Byte](-0x17, 0x53))))
        }
        case NoSuccess(msg, _) => throw new RuntimeException(msg)
      }
      Logger.println(line.toString, -1)
      if (!isNL(rest.first) && rest.first != CharSequenceReader.EofCh)
        throw new RuntimeException("Lines must be delimited")
      while (isNL(rest.first)) rest = new p.PackratReader(rest.rest)
    }
    BFuncs.flatten(bytes)
  }
}
