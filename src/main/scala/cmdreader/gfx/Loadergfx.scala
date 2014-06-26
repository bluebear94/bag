package cmdreader.gfx

import cmdreader.Global
import scala.io.Source

class Loader {
  def load = {
    val lines = Source.fromFile("docs/gfx.txt").getLines.toList
    List("ClrScn", "GetCol", "SetCol", "Test", "STC", "Line", "Rect", "Circ",
        "GetFont", "SetFont", "Text", "Poly", "PtOn", "Iter").map(Global.liblist("gfx").loadCmd(_, lines))
  }
}
