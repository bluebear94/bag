package util

import types._
import java.math.BigInteger

object BTI {
  def bti(b: Boolean): TMountain = {
    new TMountain(if (b) BigInteger.ONE else BigInteger.ZERO)
  }
  def btl(b: Boolean): THill = {
    new THill(if (b) 1L else 0L)
  }
}