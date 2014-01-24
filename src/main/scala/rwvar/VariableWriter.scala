package rwvar

import types.Type
import util._
import java.io._
import cmdreader.Global

object VariableWriter {
  def getCA(v: Type): Array[Byte] = {
    val header = Array[Byte](0x02, 0x04, -0x69, v.getType.toByte) ++
      MakeByteArrays.intToByteArray((Global.vM << 16) + (Global.vm << 8) + Global.vr)
    header ++ v.toBytecode
  }
  def writeValToVarfile(v: Type, fn: String) {
    val nf = new File(fn)
    nf.getParentFile.mkdirs
    nf.createNewFile
    val out = new FileOutputStream(nf)
    out.write(getCA(v))
    out.close
  }
}