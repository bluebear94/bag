package cmdreader.std

import cmdreader._
import types._
import gui._
import eloro._
import scala.math.BigInt

class Help extends Command {
  override def getName(): String = "help"
  override def isValidArg0(n: Int): Boolean = n <= 1
  override def apply(args: Array[Type]): Type = {
    val ZERO = TMountain(0)
    val ONE = TMountain(1)
    val TWO = TMountain(2)
    if (args.length != 0) {
      args(0) match {
        case TString(s) => Main.println(DocGen.getHelp(s))
        case ZERO => {
          Main.println("Available commands:")
          for (l <- Global.liblist) {
            Main.println("  In library " + l._1 + ":")
            var s = "    "
            for (c <- l._2.commandList) {
              s += c._1 + " "
            }
            Main.println(s)
          }
        }
        case ONE => {
          Main.println("About Bag (formerly Amethyst)\n\nWritten by bluebear94 in Scala\nWith help from toddobryan")
        }
        case TWO => {
          Main.println("""Bag is a programming language based partially on TI-Basic.
              |Assignment of new variable:      Let variable := 3
              |Assignment of existing variable: variable := 3
              |Built-in command:                $:abs(-4)
              |Function literal:                λ;(#1)^2 + 2;Endλ
              |Local variable:                  a
              |Global variable:                 $a
              |Operators:                       2+2
              |Conditionals and loops:          If (...); ...
              |(note: `;' can be substituted    If (...); Then; ......; EndIf
              | with newline                    If (...); Then; ......; Else; ......; EndIf
              | also, `...' represents one      For (...); ......; EndFor
              | expression and `......'         While (...); ......; EndWhile
              | represents any number)          Repeat (...); ......; EndRept
              |Arbitrary-precision:             5238506378105783156782697186
              |64-bit integer:                  ↼5835
              |64-bit IEEE float:               5.4; 2.; .5
              |String:                          "One line,\nand another."
              |Array:                           {"this", "was", "a", "triumph"}
              |Linked list:                     ["ti", 89, "basic", $:eq, "LAME!"]
              |Comment:                         _ don't do this on the 83+!
              |""".stripMargin)
        }
        case _ => Main.println("Unrecognized value")
      }
    }
    else {
      Main.println("""To search for a specific built-in command, pass the complete command name
          |(if the library name is std, then it may be omitted) as an argument.
          |Pass 0 as an argument to list all available commands.
          |Pass 1 as an argument to see info about this program.
          |Pass 2 as an argument to see a brief introduction to the syntax.""".stripMargin)
    }
    new TVoid
  }
}