package util


import cmdreader.Global

object PathNameConverter {
  def osToA(os: String): String = {
    val si = os.indexOf("/")
    if (si == -1) os.substring(0, os.indexOf("."))
    else os.substring(0, si) + "." + osToA(os.substring(si + 1))
  }
  def aToOs(a: String, isRoot: Boolean): (String, Boolean) = {
    val si = a.indexOf(".")
    val first = a.substring(0, Math.max(0, si))
    if (si == -1) (a + ".variable", isRoot)
    else if (first == "root") aToOs(a.substring(si + 1), true)
    else {
      val r = aToOs(a.substring(si + 1), isRoot)
      (first + "/" + r._1, r._2)
    }
  }
}