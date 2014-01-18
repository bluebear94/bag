package rwvar

import types._
import java.math.BigInteger
import java.lang.Double
import cmdreader.Global
import java.io._
import java.util.Arrays
import scala.collection.mutable._

object VariableReader {
  def tus(n: Byte) = if (n >= 0) n else 0x100 + n
  def readData(bc: Array[Byte], typeid: Int): Type = {
    typeid match {
      case 0 => new TVoid
      case 1 => new TMountain(new BigInteger(bc))
      case 2 => {
        var cumVal = 0L
        for (b <- 0 until 8) {
          cumVal = (cumVal << 8) + tus(bc(b))
        }
        new THill(cumVal)
      }
      case 3 => {
        new TString(new String(bc, "UTF-8"))
      }
      case 4 => {
        var cumVal = 0L
        for (b <- 0 until 8) {
          cumVal = (cumVal << 8) + tus(bc(b))
        }
        new TFish(Double.longBitsToDouble(cumVal))
      }
      case 5 | 6 => {
        val nElems = (tus(bc(0)) << 24) + (tus(bc(1)) << 16) + (tus(bc(2)) << 8) + tus(bc(3))
        var needle = 4
        var ce: Buffer[Type] = if (typeid == 5) new ArrayBuffer[Type]() else new ListBuffer[Type]()
        for (i <- 0 until nElems) {
          val s = (tus(bc(needle)) << 24) + (tus(bc(needle + 1)) << 16) +
            (tus(bc(needle + 2)) << 8) + tus(bc(needle + 3))
          val t = bc(needle + 4)
          val e = bc.slice(needle + 5, needle + 5 + s)
          ce.append(readData(e, t))
          needle += 5 + s
        }
        if (typeid == 5) new LArray(ce.to[ArrayBuffer]) else new LLinked(ce.to[ListBuffer])
      }
      case 7 => {
        new TBinFunc(bc, "", Global.top)
      }
      case _ => new TError(4)
    }
  }
  def readFile(fn: String) = {
    val nf = new File(fn)
    try {
      val in = new FileInputStream(nf)
      var a = Array[Byte]()
      var b = 0
      do {
        b = in.read
        if (b != -1) a = a :+ b.toByte
      } while (b != -1)
      // now a should hold the data
      // check if the first three bytes are correct
      if (a(0) != 0x02 || a(1) != 0x04 || a(2) != -0x69) new TError(4)
      else {
        val t = a(3)
        readData(a.drop(8), t)
      }
    } catch {
      case e: FileNotFoundException => new TVoid
    }
  }
}