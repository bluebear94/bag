package util

import org.scalatest._
import cmdreader.Global

object PathNameConverter {
  def osToA(os: String): String = {
    val si = os.indexOf("/")
    if (si == -1) os.substring(0, os.indexOf("."))
    else os.substring(0, si - 1) + "." + osToA(os.substring(si + 1))
  }
  def aToOs(a: String, isRoot: Boolean): (String, Boolean) = {
    val si = a.indexOf(".")
    val first = a.substring(0, si - 1)
    if (si == -1) (a + ".variable", isRoot)
    else if (first == "root") aToOs(a.substring(si + 1), true)
    else {
      val r = aToOs(a.substring(si + 1), isRoot)
      (first + "/" + r._1, r._2)
    }
  }
  class PathnameParsersTest extends FlatSpec with ShouldMatchers {
    "osToA" should "convert OS pathnames to their Amethyst counterparts" in {
      osToA("ex.variable") should equal ("ex")
      osToA("exf/ex.variable") should equal ("exf.ex")
      osToA("math/homework1/a.variable") should equal ("math.homework1.a")
    }
    "aToOs" should "convert Amethyst pathnames to their Amethyst counterparts" in {
      val c = Global.current
      val r = Global.root
      aToOs("ex", false) should equal (c + "/ex.variable")
      aToOs("exf.ex", false) should equal (c + "/exf/ex.variable")
      aToOs("root.math.homework1.a", false) should equal (r + "/math/homework1/a.variable")
    }
  }
}