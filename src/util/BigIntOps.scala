package util

import java.math.BigInteger
import types.TMountain

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
