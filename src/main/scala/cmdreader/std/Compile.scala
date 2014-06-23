package cmdreader.std

import types._
import cmdreader._
import scala.io._
import util._
import parse.ast._
import gui.Main
import rwvar._

class Compile extends Command {
  def getName = "compile"
  def isValidArg0(n: Int) = n == 1
  def apply(args: Array[Type]) = {
    val fname = args(0).toString
    val a = PathNameConverter.aToOs(fname)._1
    val addr = "amw/" + a.substring(0, a.length - 8) + "bag"
    val srcPart = Source.fromFile(addr).getLines.toList.mkString("\n")
    val src = s"\u03BB\n$srcPart\nEnd\u03BB"
    val f = WholeParser.parse(src, Main.p)
    VariableWriter.writeValToVarfile(VariableReader.readData(f.drop(6), 7, fname),
      addr.substring(0, addr.length - 3) + "variable")
    new TVoid
  }
}
