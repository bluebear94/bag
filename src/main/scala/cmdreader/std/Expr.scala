package cmdreader.std

import types._
import cmdreader._
import parse.ast._
import gui._

class Expr extends Command {
  override def getName(): String = "expr"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = {
    val code = args(0).toString
    val bytes = WholeParser.parse(code, Main.p)
    val bf = new TBinFunc(bytes, code, null, "expr - " + code)
    bf(Array())
  }
}