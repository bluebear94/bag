package cmdreader.gfx

import cmdreader.Global

class Loader {
  def load = {
    List("ClrScn", "GetCol", "SetCol", "Test", "STC", "Line", "Rect", "Circ",
        "GetFont", "SetFont", "Text").map(Global.liblist("gfx").loadCmd(_))
  }
}