package comac

import logger._

class Comachine(size: Int) {
  var mem = new Array[Byte](size)
  var reg = new Array[Long](8)
  var pc = 0
  var sp = size
  var flag = 0
  def tus(b: Byte): Int = if (b < 0) b + 256 else b
  // no fucking unsigned integers dammit
  def addy8(i: Int): Long = tus(mem(i))
  def addy16(i: Int): Long = (addy8(i) << 8) + addy8(i + 1)
  def addy32(i: Int): Long = (addy16(i) << 16) + addy16(i + 2)
  def addy64(i: Int): Long = (addy32(i) << 32) + addy32(i + 4)
  def deaddy8(i: Int, x: Long) = mem(i) = x.toByte
  def deaddy16(i: Int, x: Long) = {
    deaddy8(i, x >>> 8)
    deaddy8(i + 1, x)
  }
  def deaddy32(i: Int, x: Long) = {
    deaddy16(i, x >>> 16)
    deaddy16(i + 2, x)
  }
  def deaddy64(i: Int, x: Long) = {
    deaddy32(i, x >>> 32)
    deaddy32(i + 4, x)
  }
  def cmps(x: Long, y: Long): Int = (x - y).signum
  def cmpu(x: Long, y: Long): Int = {
    if (x < 0 && y < 0) cmps(y, x)
    else if (x < 0) 1
    else if (y < 0) -1
    else cmps(x, y)
  }
  def load(instrs: Array[Short], start: Int) {
    var addr = start
    for (instr <- instrs) {
      deaddy16(addr, instr)
      addr += 2
    }
  }
  def flag(x: Long): Unit = flag = x.signum
  def run(start: Int) {
    pc = start
    while (sp <= size) {
      val ir = addy16(pc)
      val in = ir >> 10
      val rg = ((ir >> 7) & 7).toInt
      val misc = (ir & 127).toInt
      val r2 = misc >> 4
      val off = misc & 63
      //Logger.println(s"$in $rg $misc @ $pc", -3)
      in match {
        case 0 => {
          if (sp < size - 4) pc = addy32(sp).toInt
          sp += 4
        }
        case 1 => {
          reg(rg) = reg(r2)
        }
        case 2 => {
          reg(rg) = addy64(pc + 2)
          pc += 8
        }
        case 3 => {
          reg(rg) = addy8(reg(rg).toInt)
        }
        case 4 => {
          reg(rg) = addy16(reg(rg).toInt)
        }
        case 5 => {
          reg(rg) = addy32(reg(rg).toInt)
        }
        case 6 => {
          reg(rg) = addy64(reg(rg).toInt)
        }
        case 7 => {
          reg(rg) = addy8(addy32(pc + 2).toInt)
        }
        case 8 => {
          reg(rg) = addy16(addy32(pc + 2).toInt)
        }
        case 9 => {
          reg(rg) = addy32(addy32(pc + 2).toInt)
        }
        case 10 => {
          reg(rg) = addy64(addy32(pc + 2).toInt)
        }
        case 11 => {
          deaddy8(addy32(pc + 2).toInt, reg(rg))
        }
        case 12 => {
          deaddy16(addy32(pc + 2).toInt, reg(rg))
        }
        case 13 => {
          deaddy32(addy32(pc + 2).toInt, reg(rg))
        }
        case 14 => {
          deaddy64(addy32(pc + 2).toInt, reg(rg))
        }
        case 15 => {
          reg(rg) = sp
        }
        case 16 => {
          reg(rg) += reg(r2)
          flag(reg(rg))
        }
        case 17 => {
          reg(rg) -= reg(r2)
          flag(reg(rg))
        }
        case 18 => {
          reg(rg) *= reg(r2)
        }
        case 19 => {
          val r = reg(rg) % reg(r2)
          reg(rg) /= reg(r2)
          reg(r2) = r
        }
        case 20 => {
          reg(rg) &= reg(r2)
          flag(reg(rg))
        }
        case 21 => {
          reg(rg) |= reg(r2)
          flag(reg(rg))
        }
        case 22 => {
          reg(rg) ^= reg(r2)
          flag(reg(rg))
        }
        case 23 => {
          reg(rg) = -reg(rg)
        }
        case 24 => {
          reg(rg) = ~reg(rg)
        }
        case 25 => {
          reg(rg) <<= off
        }
        case 26 => {
          flag(reg(rg) & off)
          reg(rg) >>>= off
        }
        case 27 => {
          flag((reg(rg) & off) * reg(rg).signum)
          reg(rg) >>= off
        }
        case 28 => {
          reg(rg) = (reg(rg) << misc) | (reg(rg) >>> (64 - off))
        }
        case 29 => {
          reg(rg) = (reg(rg) >>> misc) | (reg(rg) << (64 - off))
        }
        case 30 => {
          sp -= 4
          deaddy32(sp, reg(rg))
        }
        case 31 => {
          reg(rg) = addy32(sp)
          sp += 4
        }
        case 32 => {
          if ((rg & (1 << (flag + 1))) != 0) pc += (if (misc > 32) misc - 64 else misc) - 2
        }
        case 33 => {
          if ((rg & (1 << (flag + 1))) != 0) pc = addy32(pc + 2).toInt - 2
        }
        case 34 => {
          if ((rg & (1 << (flag + 1))) != 0) {
            sp -= 4
            deaddy32(sp, pc + 4)
            pc = addy32(pc + 2).toInt - 2
          } else pc += 4
        }
        case 35 => {
          if ((rg & (1 << (flag + 1))) != 0) {
            if (sp < size - 4) pc = addy32(sp).toInt
            sp += 4
          }
        }
        case 36 => {
          reg(rg) += 1
        }
        case 37 => {
          reg(rg) -= 1
        }
      }
      pc += 2
      if (in >= 7 && in <= 14) pc += 4
    }
    sp = size
  }
}

object Comachine {
  val inst = new Comachine(65535)
}
