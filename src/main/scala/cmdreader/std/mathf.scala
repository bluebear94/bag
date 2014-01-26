package cmdreader.std

import cmdreader._
import types._
import util._

class Exp extends Command {
  override def getName(): String = "exp"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.exp(args(0))
}
class Ln extends Command {
  override def getName(): String = "ln"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.ln(args(0))
}
class Pi extends Command {
  override def getName(): String = "pi"
  override def isValidArg0(n: Int): Boolean = n == 0
  override def apply(args: Array[Type]): Type = TFish(Math.PI)
}
class Sin extends Command {
  override def getName(): String = "sin"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.sin(args(0))
}
class Cos extends Command {
  override def getName(): String = "cos"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.cos(args(0))
}
class Tan extends Command {
  override def getName(): String = "tan"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.tan(args(0))
}
class ASin extends Command {
  override def getName(): String = "asin"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.asin(args(0))
}
class ACos extends Command {
  override def getName(): String = "acos"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.acos(args(0))
}
class ATan extends Command {
  override def getName(): String = "atan"
  override def isValidArg0(n: Int): Boolean = n == 1 || n == 2
  override def apply(args: Array[Type]): Type = {
    if (args.length == 1) MathUtil.atan(args(0))
    else MathUtil.atan(args(1), args(0))
  }
}
class Sinh extends Command {
  override def getName(): String = "sinh"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.sinh(args(0))
}
class Cosh extends Command {
  override def getName(): String = "cosh"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.cosh(args(0))
}
class Tanh extends Command {
  override def getName(): String = "tanh"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.tanh(args(0))
}
class ASinh extends Command {
  override def getName(): String = "asinh"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.asinh(args(0))
}
class ACosh extends Command {
  override def getName(): String = "acosh"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.acosh(args(0))
}
class ATanh extends Command {
  override def getName(): String = "atanh"
  override def isValidArg0(n: Int): Boolean = n == 1
  override def apply(args: Array[Type]): Type = MathUtil.atanh(args(0))
}