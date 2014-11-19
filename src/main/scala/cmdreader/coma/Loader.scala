package cmdreader.coma

import cmdreader.Global
import scala.io.Source

class Loader {
  def load = {
    val lines = Source.fromFile("docs/coma.txt").getLines.toList
    List("Write", "Read", "Run", "ReadR").map(Global.liblist("coma").loadCmd(_, lines))
  }
}
