package util


import cmdreader.Global

/**
 * Methods for converting between two formats: <br>
 * 
 * The Amethyst format, which uses dots to separate paths and <code>root</code> to represent the directory holding variables<br>
 * The Unix format, blah blah blah.
 * @author bluebear94
 */
object PathNameConverter {
  /**
   * Converts Unix pathnames to Amethyst pathnames.
   * @param os the Unix pathname
   * @return the corresponding Amethyst pathname
   */
  def osToA(os: String): String = {
    val si = os.indexOf("/")
    if (si == -1) os.substring(0, os.indexOf("."))
    else os.substring(0, si) + "." + osToA(os.substring(si + 1))
  }
  /**
   * Converts Amethyst pathnames to Unix pathnames.
   * @param a the Amethyst pathname
   * @param isRoot should be false; used only internally
   * @return a pair with the Unix pathname, plus true if it should refer to the root, and false if it should refer to the cd
   */
  def aToOs(a: String, isRoot: Boolean = false): (String, Boolean) = {
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