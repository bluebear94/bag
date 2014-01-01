package rwvar

import types._
import java.math.BigInteger
import java.lang.Double
import cmdreader.Global

object VariableReader {
  def tus(n: Byte) = if (n >= 0) n else 0x100 + n
  def readData(bc: Array[Byte], typeid: Int): Type = {
    typeid match {
      case 0 => new TVoid
      case 1 => new TMountain(new BigInteger(bc))
      case 2 => {
        var cumVal = 0L
        for (b <- 0 until 8) {
          cumVal = (cumVal << 8) + bc(b)
        }
        new THill(cumVal)
      }
      case 3 => {
        new TString(new String(bc, "UTF-8"))
      }
      case 4 => {
        var cumVal = 0L
        for (b <- 0 until 8) {
          cumVal = (cumVal << 8) + bc(b)
        }
        new TFish(Double.longBitsToDouble(cumVal))
      }
      case 7 => {
        new TBinFunc(bc, "", Global.top)
      }
      case _ => new TError(999)
    }
  }
}