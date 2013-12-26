package util

import types.TMountain
import java.math.BigInteger

object BTI {
  def bti(b: Boolean): TMountain = {
    new TMountain(if (b) BigInteger.ONE else BigInteger.ZERO)
  }
}