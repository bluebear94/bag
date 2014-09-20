package rwvar

import types._
import java.math.BigInteger
import java.lang.Double
import cmdreader.Global
import java.io._
import java.util.Arrays
import scala.collection.mutable.{HashMap, Buffer, ArrayBuffer, ListBuffer}
import scala.collection.immutable.Set

/**
 * Methods for reading variable data from files.
 * @author bluebear94
 */
object VariableReader {
  def tus(n: Byte) = if (n >= 0) n else 0x100 + n
  def readProtocolPart(bc: Array[Byte], start: Int = 0, accum: Set[Int] = Set()): (Set[Int], Int) = {
    val argn = (tus(bc(start)) << 8) + tus(bc(start + 1))
    argn match {
      case 0 => (accum, start + 2)
      case _ => readProtocolPart(bc, start + 2, accum + argn)
    }
  }
  /**
   * Reads data from an array of bytes, given the type identifier and the filename.
   * @param bc the bytecode, excluding the header and the length declaration
   * @param typeid the type identifier of the data
   * @param fn the file name, if any. Can be left blank but user might be confused.
   */
  def readData(bc: Array[Byte], typeid: Int, fn: String): Type = {
    typeid match {
      case 0 => TVoid.inst
      case 1 => new TMountain(new BigInteger(bc)) // not BigInt; we're reading from an array
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
          ce.append(readData(e, t, fn))
          needle += 5 + s
        }
        if (typeid == 5) new LArray(ce.to[ArrayBuffer]) else new LLinked(ce.to[ListBuffer])
      }
      case 7 => {
        val (ref, i) = readProtocolPart(bc)
        val (vel, j) = readProtocolPart(bc, i)
        new TBinFunc(bc drop j, "", Global.top, fn, FProtocol(ref, vel))
      }
      case 8 => {
        val nElems = (tus(bc(0)) << 24) + (tus(bc(1)) << 16) + (tus(bc(2)) << 8) + tus(bc(3))
        if ((nElems & 1) != 0) throw new RuntimeException("Cannot make a map out of an odd number of elements")
        def pairArgs[T](args: List[T], h: HashMap[T, T]): Unit = {
          args match {
            case a :: b :: c => {
              h += ((a, b))
              pairArgs(c, h)
            }
            case Nil => ()
            case _ => throw new RuntimeException("Cannot make a map out of an odd number of elements")
          }
        }
        var needle = 4
        var l: List[Type] = Nil
        while (needle < bc.length) {
          val size = (tus(bc(needle)) << 24) + (tus(bc(needle + 1)) << 16) + (tus(bc(needle + 2)) << 8) + tus(bc(needle + 3))
          l = readData(bc.slice(needle + 5, needle + 5 + size), bc(needle + 4), fn) :: l
          needle += 5 + size
        }
        val emptyHash = HashMap.empty[Type, Type]
        pairArgs(l.reverse, emptyHash)
        new LMap(emptyHash)
      }
      case 9 => {
        val nElems = (tus(bc(0)) << 24) + (tus(bc(1)) << 16) + (tus(bc(2)) << 8) + tus(bc(3))
        new TByteString(bc.slice(4, 4 + nElems))
      }
      case _ => new TError(4)
    }
  }
  /**
   * Reads a file.
   * @param fn the file name
   */
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
        readData(a.drop(8), t, fn)
      }
    } catch {
      case e: FileNotFoundException => TVoid.inst
    }
  }
}
