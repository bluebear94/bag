package cmdreader.std

import cmdreader.Global

class Loader {
  def load = {
    Global.liblist("std").loadCmd("OAdd")
    Global.liblist("std").loadCmd("OSubt")
    Global.liblist("std").loadCmd("OAvg")
    Global.liblist("std").loadCmd("OMult")
    Global.liblist("std").loadCmd("ODiv")
    Global.liblist("std").loadCmd("OIDiv")
    Global.liblist("std").loadCmd("OMod")
    Global.liblist("std").loadCmd("HLuna")
    Global.liblist("std").loadCmd("OEq")
    Global.liblist("std").loadCmd("ONeq")
    Global.liblist("std").loadCmd("OLt")
    Global.liblist("std").loadCmd("OLe")
    Global.liblist("std").loadCmd("OGt")
    Global.liblist("std").loadCmd("OGe")
    Global.liblist("std").loadCmd("Car")
    Global.liblist("std").loadCmd("Cdr")
    Global.liblist("std").loadCmd("Cons")
    Global.liblist("std").loadCmd("ECar")
    Global.liblist("std").loadCmd("ECdr")
    Global.liblist("std").loadCmd("ECons")
    Global.liblist("std").loadCmd("UONegate")
    Global.liblist("std").loadCmd("UONot")
    Global.liblist("std").loadCmd("OTt")
    Global.liblist("std").loadCmd("OAnd")
    Global.liblist("std").loadCmd("OOr")
    Global.liblist("std").loadCmd("OXor")
    Global.liblist("std").loadCmd("Disassembly")
  }
}