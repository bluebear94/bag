package rwvar

import types.Type
import util.MakeByteArrays
import java.io._
import cmdreader.Global

object VariableWriter {
  def getCA(v: Type): Array[Byte] = {
    val header = Array[Byte](0x02, 0x04, -0x69, v.getType.toByte) ++ MakeByteArrays.intToByteArray(0x00000001)
    header ++ v.toBytecode
  }
  def writeValToVarfile(v: Type, fn: String) {
    val nf = new File(fn)
    val out = new FileOutputStream(nf)
    out.write(getCA(v))
    out.close
  }
}