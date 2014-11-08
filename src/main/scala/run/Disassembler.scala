package run

import types._
import rwvar.VariableReader

object Disassembler {
  def formattedHex(i: Int, digits: Int = 8) = {
    var s = " "
    for (j <- 0 until digits) {
      s = "0123456789ABCDEF".charAt((((15L << (j << 2)) & i.toLong) >> (j << 2)).toInt) + s
    }
    s
  }
  
  def disassemble(bytecode: Array[Byte], indent: Int = 0, start: Int = 0): String = { // runs the bytecode
    var needle = 0
    var isDone = false
    var string = ""
    def readInt(n: Int) = {
      (tus(bytecode(n)) << 24) + (tus(bytecode(n + 1)) << 16) + (tus(bytecode(n + 2)) << 8) + tus(bytecode(n + 3))
    }
    def readString(n: Int) = {
      val length = readInt(n)
      val strSt = n + 4
      (new String(bytecode.slice(strSt, strSt + length), "UTF-8"), strSt + length)
    }
    def tus(n: Byte) = if (n >= 0) n else 0x100 + n
    while (!isDone) {
      val cmd = (tus(bytecode(needle)) << 8) + tus(bytecode(needle + 1))
      string += formattedHex(needle + start) + " " * indent
      needle += 2
      cmd match {
        case 0xE000 => {
          val argcount = readInt(needle)
          string += s"fcall $argcount"
          needle += 4
        }
        case 0xE004 => {
          val (name, newIndex) = readString(needle)
          needle = newIndex
          string += s"pv $name"
        }
        case 0xE005 => {
          val (name, newIndex) = readString(needle)
          needle = newIndex
          string += s"pvs $name"
        }
        case 0xE932 => {
          val addr = formattedHex(readInt(needle))
          string += s"jf $addr"
          needle += 4
        }
        case 0xE933 => {
          val addr = formattedHex(readInt(needle))
          string += s"jt $addr"
          needle += 4
        }
        case 0xE934 => {
          val addr = formattedHex(readInt(needle))
          string += s"j $addr"
          needle += 4
        }
        case 0xE935 => {
          string += "ret"
        }
        case 0xE938 => {
          string += "hash"
        }
        case 0xE948 => {
          string += "hashs"
        }
        case 0xE939 => {
          string += "idx"
        }
        case 0xE949 => {
          string += "idxs"
        }
        case 0xE940 => {
          val argcount = readInt(needle)
          string += s"mka $argcount"
          needle += 4
        }
        case 0xE945 => {
          val argcount = readInt(needle)
          string += s"mkl $argcount"
          needle += 4
        }
        case 0xE965 => {
          val argcount = readInt(needle)
          string += s"mkm $argcount"
          needle += 4
        }
        case 0xE950 => {
          string += "assign"
        }
        case 0xEA50 => {
          string += "assignsh"
        }
        case 0xE951 => {
          string += "dup"
        }
        case 0xE952 => {
          string += "rem"
        }
        case 0xE953 => {
          string += "setans"
        }
        case 0xE954 => string += "ans"
        case 0xE955 => string += "answer"
        case 0xE956 => {
          string += "delvar"
        }
        case 0xE957 => {
          val argcount = readInt(needle)
          string += s"revive $argcount"
        }
        case _ => {
          val hb = cmd >> 8
          val lb = cmd & 0xFF
          if (hb == 0xE1) {
            val size = readInt(needle)
            needle += 4
            if (lb == 7) {
              string += "push func where\n"
              string += disassemble(bytecode.slice(needle, needle + size), indent + 1, needle + start)
            } else {
              string += "push " + Array("void", "mt", "hl", "str", "fsh", "arr", "ll", "func").apply(lb) + " " +
               VariableReader.readData(bytecode.slice(needle, needle + size), lb, "")
              needle += size
            }
          } else if (hb == 0x75) {
            string += "pqv " + formattedHex(lb, 2)
          } else if (hb == 0x76) {
            string += "pqvs " + formattedHex(lb, 2)
          } else {
            throw new RuntimeException("Invalid command: " + cmd + "@" + (needle - 2).toHexString)
          }
        }

      }
      if (needle >= bytecode.length - 1) isDone = true
      string += "\n"
    }
    string
  }
}
