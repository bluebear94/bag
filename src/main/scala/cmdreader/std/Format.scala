package cmdreader.std

import cmdreader._
import types._

class Format extends Command {
  override def getName(): String = "format"
  override def isValidArg0(n: Int): Boolean = n >= 1
  override def apply(args: Array[Type]): Type = {
    val template = args(0).toString
    val substituted = (args.drop(1).zipWithIndex).foldLeft(template) { (accum: String, x: (Type, Int)) =>
      val (next, index) = x
      accum.replaceAll(s"\\[$index\\]", s"${next.toString}")
    }
    TString(substituted.replaceAll("\\[lb\\]", "[").replaceAll("\\[rb\\]", "]"))
  }
  override def isPure = true
}
