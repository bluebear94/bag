package cmdreader.std

import cmdreader.Global

class Loader {
  def load = {
    Global.liblist("std").loadCmd("OSubt")
    Global.liblist("std").loadCmd("OAdd")
    Global.liblist("std").loadCmd("OAvg")
    Global.liblist("std").loadCmd("OMult")
    Global.liblist("std").loadCmd("ODiv")
    Global.liblist("std").loadCmd("OIDiv")
    Global.liblist("std").loadCmd("OMod")
    Global.liblist("std").loadCmd("HLuna")
  }
}