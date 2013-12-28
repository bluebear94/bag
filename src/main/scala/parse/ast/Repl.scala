package parse.ast

import java.util.Scanner
import scala.util.parsing.input.CharSequenceReader
import cmdreader.Global
import run.RunningInstance
import scala.util.parsing.combinator._

object Repl {

  def main(args: Array[String]): Unit = {
    println("Basic Amethyst REPL\nType expressions, get results.\n(:q to exit)")
    var input: String = ""
    val s: Scanner = new Scanner(System.in)
    val p = new XprInt
    Global.loadLib("std")
    Global.liblist("std").loadCmd("OAdd")
    Global.liblist("std").loadCmd("OSubt")
    p.loadOps
    import p._
    val pp = p.phrase(p.expression)
    val ci = new RunningInstance("code: testing", null, Array())
    while (input != ":q") {
      input = s.nextLine
      if (input != ":q") {
        val tree = pp(new CharSequenceReader(input))
        println(tree)
        tree match {
          case Success(t, _) => print(t.eval(ci))
          case NoSuccess(msg, _) => print("Something wrong: " + msg)
        }

      }
      println()
    }
  }

}