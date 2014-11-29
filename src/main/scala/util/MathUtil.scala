package util
import types._
import scala.collection.mutable._
import scala.math._

/**
 * A collection of math utilities for use by Amethsyt.
 * @author bluebear94
 */

object MathUtil {
  def ttp(x: BigInt, y: BigInt): BigInt = {
    require(y >= 0)
    if (y == 0) 1
    else {
      val isRightmostSet = y.testBit(0)
      val lr = ttp(x, y >> 1)
      val lr2 = lr * lr
      if (isRightmostSet) x * lr2
      else lr2
    }
  }
  def tt(x: BigInt, y: BigInt): Type = {
    if (y >= 0) new TMountain(ttp(x, y))
    else new TFish(1.0 / ttp(x, -y).doubleValue)
  }
  def ttp(x: Long, y: Long): Long = {
    require(y >= 0L)
    if (y == 0) 1L
    else {
      val isRightMostSet = (y & 1) == 1
      val lr = ttp(x, y >> 1)
      if (isRightMostSet) x * lr * lr
      else lr * lr
    }
  }
  def tt(x: Long, y: Long): Type = {
    if (y >= 0) new THill(ttp(x, y))
    else new TFish(1.0 / ttp(x, -y))
  }
  def tt(x: Double, y: Double): Type = {
    TFish(Math.pow(x, y))
  }
  // Set of math utilities.
  /**
   * Returns the result of a method on two buffers.
   * @param f the function to apply element-by element
   * @return the resulting buffer
   */
  def motb(f: (Type, Type) => Type, l0: Buffer[Type], l1: Buffer[Type]): Buffer[Type] = {
    def fpair(p: (Type, Type)): Type = f(p._1, p._2)
    l0.zip(l1).map(fpair)
  }
  /**
   * Returns the result of a method on a buffer and a type.
   * @param f the function to apply element-by element
   * @return the resulting buffer
   */
  def moob(f: (Type, Type) => Type, l0: Buffer[Type], l1: Type): Buffer[Type] = {
    def fpair(p: Type): Type = f(p, l1)
    l0.map(fpair)
  }
  /**
   * Returns the result of a method on a buffer.
   * @param f the function to apply element-by element
   * @return the resulting buffer
   */
  def moob(f: (Type) => Type, l0: Buffer[Type]) = {
    l0.map(f)
  }
  /**
   * Returns the result of a method on two lists.
   * @param f the function to apply element-by element
   * @return the resulting list
   */
  def motl(f: (Type, Type) => Type, l0: LList, l1: LList): LList = {
    val theB = motb(f, l0.l, l1.l)
    val theT = l0.getType()
    if (theT == 5) new LArray(theB.asInstanceOf[ArrayBuffer[Type]])
    else new LLinked(theB.asInstanceOf[ListBuffer[Type]])
  }
  /**
   * Returns the result of a method on a list and an element.
   * @param f the function to apply element-by element
   * @return the resulting list
   */
  def mool(f: (Type, Type) => Type, l0: LList, l1: Type): LList = {
    val theB = moob(f, l0.l, l1)
    val theT = l0.getType()
    if (theT == 5) new LArray(theB.asInstanceOf[ArrayBuffer[Type]])
    else new LLinked(theB.asInstanceOf[ListBuffer[Type]])
  }
  /**
   * Returns the result of a method on a list.
   * @param f the function to apply element-by element
   * @return the resulting list
   */
  def mool(f: (Type) => Type, l0: LList) = {
    val theB = moob(f, l0.l)
    val theT = l0.getType()
    if (theT == 5) new LArray(theB.asInstanceOf[ArrayBuffer[Type]])
    else new LLinked(theB.asInstanceOf[ListBuffer[Type]])
  }
  def add(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType; val err = new TError(1, s"Tried to add $x and $y")
    if (xt == 3 && yt == 3) new TString(x.asInstanceOf[TString].getVal + y.asInstanceOf[TString].getVal)
    else if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(add, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(add, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(add, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal
      if (yt == 1) {
        new TMountain(bi + y.asInstanceOf[TMountain].getVal)
      } else if (yt == 2) {
        new TMountain(bi + y.asInstanceOf[THill].getVal)
      } else if (yt == 4) {
        new TFish(bi.floatValue + y.asInstanceOf[TFish].getVal)
      } else err
    } else if (xt == 2) {
      if (yt == 1) add(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal
        if (yt == 2) new THill(lg + y.asInstanceOf[THill].getVal)
        else if (yt == 4) new TFish(lg + y.asInstanceOf[TFish].getVal)
        else err
      }
    } else if (xt == 4) {
      if (yt == 1 || yt == 2) add(y, x)
      else if (yt == 4) new TFish(
        x.asInstanceOf[TFish].getVal +
          y.asInstanceOf[TFish].getVal)
      else err
    } else err
  }
  def negate(x: Type): Type = {
    val xt = x.getType()
    if (xt == 1) new TMountain(-x.asInstanceOf[TMountain].getVal)
    else if (xt == 2) new THill(-x.asInstanceOf[THill].getVal)
    else if (xt == 4) new TFish(-x.asInstanceOf[TFish].getVal)
    else if (xt == 5 || xt == 6) mool(negate(_), x.asInstanceOf[LList])
    else new TError(1, s"Tried to negate $x")
  }
  def subtract(x: Type, y: Type): Type = {
    add(x, negate(y))
  }
  def multiply(x: Type, y: Type): Type = (x, y) match {
    case (x: LList, y: LList) => motl(multiply, x, y)
    case (x: LList, y) => mool(multiply, x, y)
    case (x, y: LList) => mool(multiply, y, x)
    case (TMountain(xt), TMountain(yt)) => TMountain(xt * yt)
    case (TMountain(xt), THill(yt)) => TMountain(xt * yt)
    case (TMountain(xt), TFish(yt)) => TFish(xt.floatValue * yt)
    case (THill(xt), TMountain(yt)) => TMountain(xt * yt)
    case (THill(xt), THill(yt)) => THill(xt * yt)
    case (THill(xt), TFish(yt)) => TFish(xt * yt)
    case (TFish(xt), TMountain(yt)) => TFish(xt * yt.floatValue)
    case (TFish(xt), THill(yt)) => TFish(xt * yt)
    case (TFish(xt), TFish(yt)) => TFish(xt * yt)
    case (TString(s), x: TNumerical) => TString(s * x.intValue)
    case (x: TNumerical, TString(s)) => TString(s * x.intValue)
    case _ => new TError(1, s"Tried to multiply $x and $y")
  }
  def recip(x: Type): Type = {
    val xt = x.getType()
    if (xt == 1) new TFish(1.0 / x.asInstanceOf[TMountain].getVal.doubleValue())
    else if (xt == 2) new TFish(1.0 / x.asInstanceOf[THill].getVal)
    else if (xt == 4) new TFish(1.0 / x.asInstanceOf[TFish].getVal)
    else if (xt == 5 || xt == 6) mool(negate(_), x.asInstanceOf[LList])
    else new TError(1, s"Tried to take the reciprocal of $x")
  }
  def divide(x: Type, y: Type): Type = {
    multiply(x, recip(y))
  }
  def tt(x: Type, y: Type): Type = (x, y) match {
    case (x: LList, y: LList) => motl(tt, x, y)
    case (x: LList, y: Type) => mool(tt, x, y)
    case (x: Type, y: LList) => mool((a, b) => tt(b, a), y, x)
    case (TMountain(x), TMountain(y)) => tt(x, y)
    case (TMountain(x), THill(y)) => tt(x, y)
    case (TMountain(x), TFish(y)) => tt(x.toDouble, y)
    case (THill(x), TMountain(y)) => tt(x, y)
    case (THill(x), THill(y)) => tt(x, y)
    case (THill(x), TFish(y)) => tt(x, y)
    case (TFish(x), TMountain(y)) => tt(x, y.toDouble)
    case (TFish(x), THill(y)) => tt(x, y)
    case (TFish(x), TFish(y)) => tt(x, y)
    case (_, _) => new TError(1, s"Tried to exponentiate $x to $y")
  }
  
  /**
   * Applies a double-to-double function on an Amethyst value. Will be applied entry-by-entry on lists.
   * @param f a real-valued function to apply
   * @param x a type on which to apply this function
   * @return the result of applying f to x
   */
  def applyUnaryMath(f: (Double) => Double, x: Type): Type = {
    val xt = x.getType()
    if (xt == 1) new TFish(f(x.asInstanceOf[TMountain].getVal.doubleValue()))
    else if (xt == 2) new TFish(f(x.asInstanceOf[THill].getVal))
    else if (xt == 4) new TFish(f(x.asInstanceOf[TFish].getVal))
    else if (xt == 5 || xt == 6) mool(applyUnaryMath(f, _), x.asInstanceOf[LList])
    else new TError(1, s"Tried to apply a unary math operator on $x")
  }
  def exp(x: Type): Type = applyUnaryMath(Math.exp(_), x)
  def ln(x: Type): Type = applyUnaryMath(Math.log(_), x)
  def sin(x: Type): Type = applyUnaryMath(Math.sin(_), x)
  def cos(x: Type): Type = applyUnaryMath(Math.cos(_), x)
  def tan(x: Type): Type = applyUnaryMath(Math.tan(_), x)
  def csc(x: Type): Type = applyUnaryMath(1.0 / Math.sin(_), x)
  def sec(x: Type): Type = applyUnaryMath(1.0 / Math.cos(_), x)
  def cot(x: Type): Type = applyUnaryMath(1.0 / Math.tan(_), x)
  def asin(x: Type): Type = applyUnaryMath(Math.asin(_), x)
  def acos(x: Type): Type = applyUnaryMath(Math.acos(_), x)
  def atan(x: Type): Type = applyUnaryMath(Math.atan(_), x)
  def acsc(x: Type): Type = applyUnaryMath(y => Math.asin(1.0 / y), x)
  def asec(x: Type): Type = applyUnaryMath(y => Math.acos(1.0 / y), x)
  def acot(x: Type): Type = applyUnaryMath(Math.PI / 2 - Math.atan(_), x)
  def sinh(x: Type): Type = applyUnaryMath(Math.sinh(_), x)
  def cosh(x: Type): Type = applyUnaryMath(Math.cosh(_), x)
  def tanh(x: Type): Type = applyUnaryMath(Math.tanh(_), x)
  def csch(x: Type): Type = applyUnaryMath(1.0 / Math.sinh(_), x)
  def sech(x: Type): Type = applyUnaryMath(1.0 / Math.cosh(_), x)
  def coth(x: Type): Type = applyUnaryMath(1.0 / Math.tanh(_), x)
  def asinh(x: Double) = Math.log(Math.hypot(1.0, x) + x)
  def acosh(x: Double) = Math.log(Math.sqrt(Math.pow(x, 2) - 1.0) + x)
  def atanh(x: Double) = 0.5 * (Math.log(x + 1) - Math.log(1 - x))
  def asinh(x: Type): Type = applyUnaryMath(asinh(_), x)
  def acosh(x: Type): Type = applyUnaryMath(acosh(_), x)
  def atanh(x: Type): Type = applyUnaryMath(atanh(_), x)
  def acsch(x: Type): Type = applyUnaryMath(y => asinh(1.0 / y), x)
  def asech(x: Type): Type = applyUnaryMath(y => acosh(1.0 / y), x)
  def acoth(x: Type): Type = applyUnaryMath(y => 0.5 * Math.log((y + 1)/(y - 1)), x)
  def atan(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(atan(_, _), xl, yl)
      case (xl: LList, _) => mool(atan(_, _), xl, y)
      case (_, yl: LList) => mool((a, b) => atan(b, a), yl, x)
      case (x: TNumerical, y: TNumerical) => TFish(Math.atan2(y.doubleValue, x.doubleValue))
      case _ => new TError(1)
    }
  }
  def idivide(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType
    if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(idivide, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(idivide, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(idivide, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal
      if (yt == 1) {
        new TMountain(bi/ y.asInstanceOf[TMountain].getVal)
      } else if (yt == 2) {
        new TMountain(bi / y.asInstanceOf[THill].getVal)
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) idivide(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal
        if (yt == 2) new THill(lg / y.asInstanceOf[THill].getVal)
        else new TError(1)
      }
    } else new TError(1)
  }
  def mod(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType
    if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(mod, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(mod, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(mod, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal
      if (yt == 1) {
        new TMountain(bi % y.asInstanceOf[TMountain].getVal)
      } else if (yt == 2) {
        new TMountain(bi & y.asInstanceOf[THill].getVal)
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) mod(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal
        if (yt == 2) new THill(lg % y.asInstanceOf[THill].getVal)
        else new TError(1)
      }
    } else new TError(1)
  }
  /**
   * Applies a relational operator on two types.
   * @param x the first type
   * @param y the second type
   * @param f the relation function
   * @return the result of applying f to x and y, applying entry-by-entry if necessary
   */
  def rel(x: Type, y: Type, f: (Type, Type) => Boolean): Type = {
    val xt = x.getType; val yt = y.getType
    def g(a: Type, b: Type): Type = {
      BTI.bti(f(a, b))
    }
    if (xt == 5 || xt == 6) {
      if (yt == 5 || yt == 6)
        motl(g, x.asInstanceOf[LList], y.asInstanceOf[LList])
      else
        mool(g, x.asInstanceOf[LList], y)
    } else {
      if (yt == 5 || yt == 6)
        mool((a, b) => g(b, a), y.asInstanceOf[LList], x)
      else {
        g(x, y)
      }
    }
  }
  def notP(x: Type): Type = {
    BTI.bti(!x.toBoolean)
  }
  def notnotP(x: Type): Type = {
    BTI.bti(x.toBoolean)
  }
  def not(x: Type) = {
    x match {
      case t: LList => mool(notP(_), t)
      case _ => notP(x)
    }
  }
  def notnot(x: Type) = {
    x match {
      case t: LList => mool(notnotP(_), t)
      case _ => notnotP(x)
    }
  }
  def andNSC(x: Type, y: Type): Type = {
    x match {
      case xt: LList => y match {
        case yt: LList => motl(andNSC(_, _), xt, yt)
        case _ => mool(andNSC(_, _), xt, y)
      }
      case _ => y match {
        case yt: LList => mool(andNSC(_, _), yt, x)
        case _ => BTI.bti(x.toBoolean && y.toBoolean)
      }
    }
  }
  def orNSC(x: Type, y: Type): Type = {
    x match {
      case xt: LList => y match {
        case yt: LList => motl(orNSC(_, _), xt, yt)
        case _ => mool(orNSC(_, _), xt, y)
      }
      case _ => y match {
        case yt: LList => mool(orNSC(_, _), yt, x)
        case _ => BTI.bti(x.toBoolean || y.toBoolean)
      }
    }
  }
  def shl(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(shl(_, _), xl, yl)
      case (xl: LList, _) => mool(shl(_, _), xl, y)
      case (_, yl: LList) => mool((a, b) => shl(b, a), yl, x)
      case (xm: TMountain, ym: TNumerical) => {
        new TMountain(xm.getVal << ym.intValue)
      }
      case (xm: THill, ym: TNumerical) => {
        new THill(xm.getVal << ym.intValue)
      }
      case (xm: TFish, ym: TNumerical) => {
        new TFish(xm.getVal * Math.pow(2, ym.intValue))
      }
      case (_, _) => new TError(1)
    }
  }
  def shr(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(shr(_, _), xl, yl)
      case (xl: LList, _) => mool(shr(_, _), xl, y)
      case (_, yl: LList) => mool((a, b) => shr(b, a), yl, x)
      case (xm: TMountain, ym: TNumerical) => {
        new TMountain(xm.getVal >> ym.intValue)
      }
      case (xm: THill, ym: TNumerical) => {
        new THill(xm.getVal >> ym.intValue)
      }
      case (xm: TFish, ym: TNumerical) => {
        new TFish(xm.getVal * Math.pow(2, -ym.intValue))
      }
      case (_, _) => new TError(1)
    }
  }
  def rol(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(rol(_, _), xl, yl)
      case (xl: LList, _) => mool(rol(_, _), xl, y)
      case (_, yl: LList) => mool((a, b) => rol(b, a), yl, x)
      case (xm: TMountain, ym: TNumerical) => {
        new TMountain(rol(xm.getVal, ym.intValue))
      }
      case (xm: THill, ym: TNumerical) => {
        new THill(rol(xm.getVal, ym.intValue))
      }
      case (_, _) => new TError(1)
    }
  }
  def ror(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(ror(_, _), xl, yl)
      case (xl: LList, _) => mool(ror(_, _), xl, y)
      case (_, yl: LList) => mool((a, b) => ror(b, a), yl, x)
      case (xm: TMountain, ym: TNumerical) => {
        new TMountain(ror(xm.getVal, ym.intValue))
      }
      case (xm: THill, ym: TNumerical) => {
        new THill(ror(xm.getVal, ym.intValue))
      }
      case (_, _) => new TError(1)
    }
  }
  def bitAnd(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(bitAnd(_, _), xl, yl)
      case (xl: LList, _) => mool(bitAnd(_, _), xl, y)
      case (_, yl: LList) => bitAnd(y, x)
      case (xm: TMountain, ym: TMountain) => new TMountain(xm.getVal & ym.getVal)
      case (xm: TMountain, ym: THill) => new TMountain(xm.getVal & ym.getVal)
      case (xm: TMountain, ym: TFish) => new TMountain(xm.getVal &
          BigDecimal(ym.getVal).toBigInt) // yikes
      case (xm: THill, ym: TMountain) => bitAnd(y, x)
      case (xm: THill, ym: THill) => new THill(xm.getVal & ym.getVal)
      case (xm: THill, ym: TFish) => new TMountain(xm.getVal &
          BigDecimal(ym.getVal).toBigInt)
      case (xm: TFish, ym: TMountain) => bitAnd(y, x)
      case (xm: TFish, ym: THill) => bitAnd(y, x)
      case (xm: TFish, ym: TFish) => new TMountain(BigDecimal(xm.getVal).toBigInt &
          BigDecimal(ym.getVal).toBigInt)
      case (_, _) => new TError(1)
    }
  }
  def bitOr(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(bitOr(_, _), xl, yl)
      case (xl: LList, _) => mool(bitOr(_, _), xl, y)
      case (_, yl: LList) => bitOr(y, x)
      case (xm: TMountain, ym: TMountain) => new TMountain(xm.getVal | ym.getVal)
      case (xm: TMountain, ym: THill) => new TMountain(xm.getVal | ym.getVal)
      case (xm: TMountain, ym: TFish) => new TMountain(xm.getVal |
          BigDecimal(ym.getVal).toBigInt)
      case (xm: THill, ym: TMountain) => bitOr(y, x)
      case (xm: THill, ym: THill) => new THill(xm.getVal | ym.getVal)
      case (xm: THill, ym: TFish) => new TMountain(xm.getVal |
          BigDecimal(ym.getVal).toBigInt)
      case (xm: TFish, ym: THill) => bitOr(y, x)
      case (xm: TFish, ym: TFish) => new TMountain(BigDecimal(xm.getVal).toBigInt |
          BigDecimal(ym.getVal).toBigInt)
      case (_, _) => new TError(1)
    }
  }
  def bitXor(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(bitXor(_, _), xl, yl)
      case (xl: LList, _) => mool(bitXor(_, _), xl, y)
      case (_, yl: LList) => bitXor(y, x)
      case (xm: TMountain, ym: TMountain) => new TMountain(xm.getVal ^ ym.getVal)
      case (xm: TMountain, ym: THill) => new TMountain(xm.getVal ^ ym.getVal)
      case (xm: TMountain, ym: TFish) => new TMountain(xm.getVal ^
          BigDecimal(ym.getVal).toBigInt)
      case (xm: THill, ym: TMountain) => bitXor(y, x)
      case (xm: THill, ym: THill) => new THill(xm.getVal ^ ym.getVal)
      case (xm: THill, ym: TFish) => new TMountain(xm.getVal ^
          BigDecimal(ym.getVal).toBigInt)
      case (xm: TFish, ym: TMountain) => bitXor(y, x)
      case (xm: TFish, ym: THill) => bitXor(y, x)
      case (xm: TFish, ym: TFish) => new TMountain(BigDecimal(xm.getVal).toBigInt ^
          BigDecimal(ym.getVal).toBigInt)
      case (_, _) => new TError(1)
    }
  }
  def abs(x: Type): Type = {
    x match {
      case xm: TMountain => new TMountain(xm.getVal.abs)
      case xh: THill => new THill(Math.abs(xh.getVal))
      case xf: TFish => new TFish(Math.abs(xf.getVal))
      case xl: LList => mool(abs(_), xl)
      case _ => new TError(1)
    }
  }
  def floor(x: Type): Type = {
    x match {
      case xm: TMountain => new TMountain(xm.getVal)
      case xh: THill => new THill(Math.abs(xh.getVal))
      case xf: TFish => new TMountain(new scala.math.BigDecimal(new java.math.BigDecimal(xf.getVal)).toBigInt)
      case xl: LList => mool(floor(_), xl)
      case _ => new TError(1)
    }
  }
  def fpart(x: Double) = x - x.floor
  def fpart(x: Type): Type = {
    x match {
      case xm: TMountain => new TMountain(0)
      case xh: THill => new THill(0L)
      case xf: TFish => new TFish(fpart(xf.getVal))
      case xl: LList => mool(abs(_), xl)
      case _ => new TError(1)
    }
  }
  def rol(x: Long, p: Int) = (x << p) | (x >>> (64 - p))
  def ror(x: Long, p: Int) = (x >>> p) | (x << (64 - p))
  def rol(x: BigInt, p: Int) = (x << p) | (x >> (x.bitLength - p))
  def ror(x: BigInt, p: Int) = (x >> p) | (x << (x.bitLength - p))
  def digit(d: Int) = (d + (if (d <= 9) '0' else 'W')).toChar
  def toStringRadix(x: Double, b: Int) = {
    var p = x.abs
    var q = p.toInt
    p -= q
    var s = ""
    while (q != 0) {
      s = digit(q % b) + s
      q /= b
    }
    if (s == "") s = "0"
    s += "."
    var i = 0
    while (i < 16 && p != 0) {
      p *= b
      s += digit(p.toInt)
      p -= p.toInt
      i += 1
    }
    s
  }
}
