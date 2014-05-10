package cmdreader.gfx

import cmdreader.Global

class Loader {
  def load = {
    List("ClrScn", "GetCol", "SetCol").map(Global.liblist("gfx").loadCmd(_))
  }
}