package cmdreader.gfx

import cmdreader._
import types._
import util._

class Poly extends Command {
  def getName() = "poly"
  def isValidArg0(n: Int) = n == 3
  def apply(args: Array[Type]): Type = {
    val xs = args(0) match {
      case l: LList => {
        l.l.toArray map {
          case n: TNumerical => n.intValue
          case _ => return new TError(1, "#1 must be a list of numbers")
        }
      }
      case _ => return new TError(1, "#1 must be list of numbers")
    }
    val ys = args(1) match {
      case l: LList => {
      l.l.toArray map {
          case n: TNumerical => n.intValue
          case _ => return new TError(1, "#2 must be a list of numbers")
        }
      }
      case _ => return new TError(1, "#2 must be list of numbers")
    }
    GFX.poly(xs,
      ys,
      args(2).toBoolean)
    new TVoid
  }
}
