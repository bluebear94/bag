package cmdreader.gfx

import cmdreader._
import types._
import util._

class Iter extends Command {
  def getName() = "iter"
  def isValidArg0(n: Int) = n == 1 || n == 2 || n == 6
  def apply(args: Array[Type]): Type = {
    val f = args(0) match {
      case f: FuncLike => (x: Int, y: Int) => GFX.getIntOrChoke(f(Array(THill(x), THill(y))))
      case _ => return new TError(1, "first argument must be function")
    }
    val a = if (args.length > 1) args(1).toBoolean else false
    val win = if (args.length == 6) args.drop(2).map(GFX.getIntOrChoke(_)) else Array(0, 0, 640, 480)
    GFX.iterate(f, a,
      win(0), win(1), win(2), win(3))
    new TVoid
  }
}
