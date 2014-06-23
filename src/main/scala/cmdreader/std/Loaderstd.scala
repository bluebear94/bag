package cmdreader.std

import cmdreader.Global
import scala.io.Source

class Loader {
  def load = {
    val lines = Source.fromFile("docs/std.txt").getLines.toList
    List("OAdd", "OSubt", "OAvg", "OMult", "ODiv", "OIDiv", "OMod", "HLuna", "OEq", "ONeq", "OLt", "OLe", "OGt",
        "OGe", "Car", "Cdr", "Cons", "ECar", "ECdr", "ECons", "UONegate", "UONot", "OTt", "OAnd", "OOr", "OXor",
        "Disassembly", "OAndB", "OOrB", "OXorB", "Now", "Sub", "Rrbzo", "Abs", "Floor", "FPart", "OShl", "OShr",
        "OMap", "Exp", "Ln", "Pi", "Expr", "FoldL", "FoldR", "OFilter", "HSort", "Str", "Len", "QSort", "MSort",
        "Ribzpt", "OAug", "Help", "HAsk", "Clone", "ValCopy", "GetType", "OSeq", "Yield", "Stib", "Itsb", "Require",
        "In", "Idx", "Isc", "Ixsc", "Cast", "Idxe", "Idxl", "Ixsce", "Ixscl", "Compile", "KeyValue", "ORol", "ORor").map(Global.liblist("std").loadCmd(_, lines))
    List("Sin", "Cos", "Tan").map({
      s => List(s, s + "h", "A" + s, "A" + s + "h").map(Global.liblist("std").loadCmd(_, lines))
    })
  }
}
