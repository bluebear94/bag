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
          Main.println("About Amethyst\n\nWritten by bluebear94 in Scala\nWith help from [name withheld]")
        }
        case _ => Main.println("Unrecognized value")
      }
    }
    else {
      Main.println("""To search for a specific built-in command, pass the complete command name
          |(if the library name is std, then it may be omitted) as an argument.
          |Pass 0 as an argument to list all available commands.
          |Pass 1 as an argument to see info about this program.""".stripMargin)
    }
    new TVoid
  }
}