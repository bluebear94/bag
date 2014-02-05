package util

import java.math.BigInteger
import types.TMountain

/**
 * Object to facilitate comparisons with BigIntegers.
 */
@deprecated("Now unnecessary due to scala.math.BigInt being used for the arbitrary-precision integer implementation.", "0.5.13")
object BigIntOps {
  def fits(x: BigInteger, y: BigInteger, p: (Int) => Boolean) = {
	p(x.compareTo(y))
  }
  def makeFF(p: (Int) => Boolean): (BigInteger, BigInteger) => Boolean = {
    (x: BigInteger, y: BigInteger) =>
      fits(x, y, p)
  }
  val gt = makeFF((x) => x > 0)
  val lt = makeFF((x) => x < 0)
  val ge = makeFF((x) => x >= 0)
  val le = makeFF((x) => x <= 0)
}

