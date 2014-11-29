package cmdreader.std

import cmdreader._
import types._
import scala.math.BigInt

class Ritb extends Command {
  override def getName(): String = "ritb"
  override def isValidArg0(n: Int): Boolean = n == 2
  override def apply(args: Array[Type]): Type = {
    val lb = args(0).intValue
    val ub = args(1).intValue
    TMountain(Ritb.randomBigInt(ub - lb) + lb)
  }
}
object Ritb {
  def randomBigInt(max: BigInt): BigInt = randomBigInt(max, max.bitLength)
  def randomBigInt(max: BigInt, l: Int): BigInt = {
    val x = BigInt(l, Global.r)
    if (x < max) x else randomBigInt(max, l)
  }
}
