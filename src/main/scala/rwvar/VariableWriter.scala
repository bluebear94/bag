package rwvar

import types.Type
import util._
import java.io._
import cmdreader.Global

/**
 * Methods for writing variables.
 * @author bluebear94
 */
object VariableWriter {
  /**
   * Gets the complete bytecode of the value, including the header and type declarations.
   * @param v the value to convert into a bytecode representation
   * @return the corresponding binary representation, including the header and type declarations
   */
  def getCA(v: Type): Array[Byte] = {
    val header = Array[Byte](0x02, 0x04, -0x69, v.getType.toByte) ++
      MakeByteArrays.intToByteArray((Global.vM << 16) + (Global.vm << 8) + Global.vr)
    header ++ v.toBytecode
  }
  /**
   * Writes a value to file.
   * @param v the value to save
   * @param fn the filename
   */
  def writeValToVarfile(v: Type, fn: String) {
    val nf = new File(fn)
    nf.getParentFile.mkdirs
    nf.createNewFile
    val out = new FileOutputStream(nf)
    out.write(getCA(v))
    out.close
  }
}