package cmdreader.std

import cmdreader._
import types._
import gui._
import eloro._
import scala.math.BigInt
import scala.io.Source

class Help extends Command {
  override def getName(): String = "help"
  override def isValidArg0(n: Int): Boolean = n <= 1
  override def apply(args: Array[Type]): Type = {
    val ZERO = TMountain(0)
    val ONE = TMountain(1)
    val TWO = TMountain(2)
    val THREE = TMountain(3)
    val FOUR = TMountain(4)
    val FIVE = TMountain(5)
    if (args.length != 0) {
      args(0) match {
        case TString(s) => Main.println(DocGen.getHelp(s))
        case ZERO => {
          Main.println("Available libraries:")
          for (l <- Global.liblist) {
            Main.println(l._1)
          }
        }
        case ONE => {
          Main.println("""About Bag (formerly Amethyst)
              |
              |Written by bluebear94 in Scala
              |With help from toddobryan
              |
              |The project was first a concept of a programming language true to TI-Basic.
              |By October, some classes had been implemented in Java (under the codename Labyrinth), but the project became too complex.
              |In December, the project was rewritten in Scala, taking advantage of the powerful parser combinator library.
              |""".stripMargin)
        }
        case TWO => {
          Main.println("""Bag is a programming language based partially on TI-Basic.
              |Assignment of new variable:      Let variable := 3
              |Assignment of existing variable: variable := 3
              |Built-in command:                $:abs(-4)
              |Library command:                 $gfx:poly({100, 300, 150}, {100, 150, 300}, 1)
              |Use of operators:                4 + a _ equivalent to $:add(4, a)
              |Function literal:                λ; (#1)^2 + 2; Endλ
              |Closures:                        Map("this" → λ; #1 + a; Endλ, "a" → 2 * q * pi())
              |Local variable:                  a
              |Global variable:                 $a
              |Operators:                       2+2
              |Conditionals and loops:          If (...); ...
              |(note: `;' can be substituted    If (...); Then; ......; EndIf
              | with newline                    If (...); Then; ......; Else; ......; EndIf
              | also, `...' represents one      For (...); ......; EndFor
              | expression and `......'         While (...); ......; EndWhile
              | represents any number of them)  Repeat (...); ......; EndRept
              |Arbitrary-precision:             5238506378105783156782697186
              |64-bit integer:                  5835H (old syntax: ↼5835)
              |64-bit IEEE float:               5.4; 2.; .5
              |String:                          "One line,\nand another."
              |Array:                           {"this", "was", "a", "triumph"}
              |Linked list:                     ["ti", 89, "basic", $:eq, "LAME!"]
              |Comment:                         _ don't do this on the 83+!
              |Closing parentheses, brackets, braces, and quotes are optional before a newline.
              |""".stripMargin)
        }
        case THREE => {
          val lname = args(1).toString
          val ll = Global.liblist(lname)
          Main.println("  In library " + lname + ":")
          Main.println(DocGen.getLibInfo(lname))
          var s = "    "
          for (c <- ll.commandList) {
            s += c._1 + " "
          }
          Main.println(s)
        }
        case FOUR => {
          Source.fromFile("docs/style.txt").getLines foreach Main.println
          Main.println("\nThis guide is also accessible in docs/style.txt.")
        }
        case FIVE => {
          DocGen.genLibXS(args(1).toString)
        }
        case _ => Main.println("Unrecognized value")
      }
    } else {
      Main.println("""To search for a specific built-in command, pass the complete command name
          |(if the library name is std, then it may be omitted) as an argument.
          |Pass 0 as an argument to list all loaded libraries.
          |Pass 1 as an argument to see info about this program.
          |Pass 2 as an argument to see a brief introduction to the syntax.
          |Pass 3 and a library name to see all commands in a given library.
          |Pass 4 to view the Bag style guide.
          |Pass 5 to generate an HTML page containing library info.""".stripMargin)
    }
    TVoid.inst
  }
}
