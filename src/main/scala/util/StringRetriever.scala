package util

import java.io._
import scala.collection.mutable.StringBuilder

/*
 * Retrieves a string from a Reader.
 * @author bb94
 */
@deprecated("Found out about scala.io.Source.", "0.7.2")
object StringRetriever {
  def getStringFrom(r: Reader) = {
    val cb = new StringBuilder
    var cr = 0
    while (cr != -1) {
      cr = r.read
      println(cr)
      if (cr != -1) cb += cr.toChar
    }
    cb.result
  }
}
