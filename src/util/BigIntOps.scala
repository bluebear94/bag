package util

import java.math.BigInteger

object BigIntOps {
  def fits(x: BigInteger, y: BigInteger, p: (Int) => Boolean): Boolean = {
    p(x.compareTo(y))
  }
  def makeFitter(p: (Int) => Boolean): (BigInteger, BigInteger) => Boolean = {
    (x: BigInteger, y: BigInteger) =>
      fits(x, y, p)
  }
  val gt = makeFitter((i) => (i > 0))
  val ge = makeFitter((i) => (i >= 0))
  val lt = makeFitter((i) => (i < 0))
  val le = makeFitter((i) => (i <= 0))
}