package cmdreader.std

import cmdreader.Global

class Loader {
  def load = {
    Global.liblist("std").loadCmd("OSubt")
    Global.liblist("std").loadCmd("OAdd")
  }
}