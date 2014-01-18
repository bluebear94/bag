package util
import types._
import scala.collection.mutable._
import java.math._

object MathUtil {
  def ttp(x: BigInteger, y: BigInteger): BigInteger = {
    require(y.compareTo(BigInteger.ZERO) >= 0)
    if (y.equals(BigInteger.ZERO)) BigInteger.ONE
    else {
      val isRightmostSet = y.testBit(0)
      val lr = ttp(x, y.shiftRight(1))
      if (isRightmostSet) x.multiply(lr.multiply(lr))
      else lr.multiply(lr)
    }
  }
  def tt(x: BigInteger, y: BigInteger): Type = {
    if (y.compareTo(BigInteger.ZERO) >= 0) new TMountain(ttp(x, y))
    else new TFish(1.0 / ttp(x, y.negate).doubleValue)
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
  // Set of math utilities.
  def motb(f: (Type, Type) => Type, l0: Buffer[Type], l1: Buffer[Type]): Buffer[Type] = {
    def fpair(p: (Type, Type)): Type = f(p._1, p._2)
    l0.zip(l1).map(fpair)
  }
  def moob(f: (Type, Type) => Type, l0: Buffer[Type], l1: Type): Buffer[Type] = {
    def fpair(p: Type): Type = f(p, l1)
    l0.map(fpair)
  }
  def moob(f: (Type) => Type, l0: Buffer[Type]) = {
    l0.map(f)
  }
  def motl(f: (Type, Type) => Type, l0: LList, l1: LList): LList = {
    val theB = motb(f, l0.l(), l1.l())
    val theT = l0.getType()
    if (theT == 5) new LArray(theB.asInstanceOf[ArrayBuffer[Type]])
    else new LLinked(theB.asInstanceOf[ListBuffer[Type]])
  }
  def mool(f: (Type, Type) => Type, l0: LList, l1: Type): LList = {
    val theB = moob(f, l0.l(), l1)
    val theT = l0.getType()
    if (theT == 5) new LArray(theB.asInstanceOf[ArrayBuffer[Type]])
    else new LLinked(theB.asInstanceOf[ListBuffer[Type]])
  }
  def mool(f: (Type) => Type, l0: LList) = {
    val theB = moob(f, l0.l())
    val theT = l0.getType()
    if (theT == 5) new LArray(theB.asInstanceOf[ArrayBuffer[Type]])
    else new LLinked(theB.asInstanceOf[ListBuffer[Type]])
  }
  def add(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType
    if (xt == 3 && yt == 3) new TString(x.asInstanceOf[TString].getVal() + y.asInstanceOf[TString].getVal())
    else if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(add, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(add, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(add, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal()
      if (yt == 1) {
        new TMountain(bi.add(y.asInstanceOf[TMountain].getVal()))
      } else if (yt == 2) {
        new TMountain(bi.add(new BigInteger(y.asInstanceOf[THill].getVal().toString())))
      } else if (yt == 4) {
        new TFish(bi.floatValue() + y.asInstanceOf[TFish].getVal())
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) add(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal()
        if (yt == 2) new THill(lg + y.asInstanceOf[THill].getVal())
        else if (yt == 4) new TFish(lg + y.asInstanceOf[TFish].getVal())
        else new TError(1)
      }
    } else if (xt == 4) {
      if (yt == 1 || yt == 2) add(y, x)
      else if (yt == 4) new TFish(
        x.asInstanceOf[TFish].getVal() +
          y.asInstanceOf[TFish].getVal())
      else new TError(1)
    } else new TError(1)
  }
  def negate(x: Type): Type = {
    val xt = x.getType()
    if (xt == 1) new TMountain(x.asInstanceOf[TMountain].getVal().negate())
    else if (xt == 2) new THill(-x.asInstanceOf[THill].getVal())
    else if (xt == 4) new TFish(-x.asInstanceOf[TFish].getVal())
    else if (xt == 5 || xt == 6) mool(negate(_), x.asInstanceOf[LList])
    else new TError(1)
  }
  def subtract(x: Type, y: Type): Type = {
    add(x, negate(y))
  }
  def multiply(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType
    if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(multiply, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(multiply, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(multiply, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal()
      if (yt == 1) {
        new TMountain(bi.multiply(y.asInstanceOf[TMountain].getVal()))
      } else if (yt == 2) {
        new TMountain(bi.multiply(new BigInteger(y.asInstanceOf[THill].getVal().toString())))
      } else if (yt == 4) {
        new TFish(bi.floatValue() * y.asInstanceOf[TFish].getVal())
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) multiply(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal()
        if (yt == 2) new THill(lg * y.asInstanceOf[THill].getVal())
        else if (yt == 4) new TFish(lg * y.asInstanceOf[TFish].getVal())
        else new TError(1)
      }
    } else if (xt == 4) {
      if (yt == 1 || yt == 2) multiply(y, x)
      else if (yt == 4) new TFish(
        x.asInstanceOf[TFish].getVal() *
          y.asInstanceOf[TFish].getVal())
      else new TError(1)
    } else new TError(1)
  }
  def recip(x: Type): Type = {
    val xt = x.getType()
    if (xt == 1) new TFish(1.0 / x.asInstanceOf[TMountain].getVal().doubleValue())
    else if (xt == 2) new TFish(1.0 / x.asInstanceOf[THill].getVal())
    else if (xt == 4) new TFish(1.0 / x.asInstanceOf[TFish].getVal())
    else if (xt == 5 || xt == 6) mool(negate(_), x.asInstanceOf[LList])
    else new TError(1)
  }
  def divide(x: Type, y: Type): Type = {
    multiply(x, recip(y))
  }
  def tt(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType
    if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(tt, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(tt, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(tt, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal()
      if (yt == 1) {
        tt(bi, (y.asInstanceOf[TMountain].getVal()))
      } else if (yt == 2) {
        tt(bi, BigInteger.valueOf(y.asInstanceOf[THill].getVal))
      } else if (yt == 4) {
        new TFish(Math.pow(bi.floatValue(), y.asInstanceOf[TFish].getVal()))
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) tt(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal()
        if (yt == 2) tt(lg, y.asInstanceOf[THill].getVal)
        else if (yt == 4) new TFish(Math.pow(lg, y.asInstanceOf[TFish].getVal()))
        else new TError(1)
      }
    } else if (xt == 4) {
      if (yt == 1 || yt == 2) tt(y, x)
      else if (yt == 4) new TFish(Math.pow(
        x.asInstanceOf[TFish].getVal(),
          y.asInstanceOf[TFish].getVal()))
      else new TError(1)
    } else new TError(1)
  }
  def applyUnaryMath(f: (Double) => Double, x: Type): Type = {
    val xt = x.getType()
    if (xt == 1) new TFish(f(x.asInstanceOf[TMountain].getVal().doubleValue()))
    else if (xt == 2) new TFish(f(x.asInstanceOf[THill].getVal()))
    else if (xt == 4) new TFish(f(x.asInstanceOf[TFish].getVal()))
    else if (xt == 5 || xt == 6) mool(applyUnaryMath(f, _), x.asInstanceOf[LList])
    else new TError(1)
  }
  def idivide(x: Type, y: Type): Type = {
    val xt = x.getType; val yt = y.getType
    if ((xt == 5 || xt == 6) && (yt == 5 || yt == 6)) motl(idivide, x.asInstanceOf[LList], y.asInstanceOf[LList])
    else if ((xt == 5 || xt == 6)) mool(idivide, x.asInstanceOf[LList], y)
    else if ((yt == 5 || yt == 6)) mool(idivide, y.asInstanceOf[LList], x)
    else if (xt == 1) {
      val bi = x.asInstanceOf[TMountain].getVal()
      if (yt == 1) {
        new TMountain(bi.divide(y.asInstanceOf[TMountain].getVal()))
      } else if (yt == 2) {
        new TMountain(bi.divide(new BigInteger(y.asInstanceOf[THill].getVal().toString())))
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) idivide(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal()
        if (yt == 2) new THill(lg / y.asInstanceOf[THill].getVal())
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
      val bi = x.asInstanceOf[TMountain].getVal()
      if (yt == 1) {
        new TMountain(bi.mod(y.asInstanceOf[TMountain].getVal()))
      } else if (yt == 2) {
        new TMountain(bi.mod(new BigInteger(y.asInstanceOf[THill].getVal().toString())))
      } else new TError(1)
    } else if (xt == 2) {
      if (yt == 1) mod(y, x)
      else {
        val lg = x.asInstanceOf[THill].getVal()
        if (yt == 2) new THill(lg % y.asInstanceOf[THill].getVal())
        else new TError(1)
      }
    } else new TError(1)
  }
  def rel(x: Type, y: Type, f: (Type, Type) => Boolean): Type = {
    val xt = x.getType; val yt = y.getType
    def g(a: Type, b: Type): Type = {
      BTI.bti(f(a, b))
    }
    if (xt == 5 || xt == 6) {
      if (yt == 5 || yt == 6)
        motl(g, x.asInstanceOf[LList], y.asInstanceOf[LList])
      else
        mool(g, x.asInstanceOf[LList], y.asInstanceOf[LList])
    } else {
      if (yt == 5 || yt == 6)
        mool(g, y.asInstanceOf[LList], x.asInstanceOf[LList])
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
      case (_, yl: LList) => shl(y, x)
      case (xm: TMountain, ym: TNumerical) => {
        new TMountain(xm.getVal.shiftLeft(ym.intValue))
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
      case (_, yl: LList) => shr(y, x)
      case (xm: TMountain, ym: TNumerical) => {
        new TMountain(xm.getVal.shiftRight(ym.intValue))
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
  def bitAnd(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(bitAnd(_, _), xl, yl)
      case (xl: LList, _) => mool(bitAnd(_, _), xl, y)
      case (_, yl: LList) => bitAnd(y, x)
      case (xm: TMountain, ym: TMountain) => new TMountain(xm.getVal and ym.getVal)
      case (xm: TMountain, ym: THill) => new TMountain(xm.getVal and BigInteger.valueOf(ym.getVal))
      case (xm: TMountain, ym: TFish) => new TMountain(xm.getVal and new BigDecimal(ym.getVal).toBigInteger)
      case (xm: THill, ym: TMountain) => bitAnd(y, x)
      case (xm: THill, ym: THill) => new THill(xm.getVal & ym.getVal)
      case (xm: THill, ym: TFish) => new TMountain(BigInteger.valueOf(xm.getVal) and new BigDecimal(ym.getVal).toBigInteger)
      case (xm: TFish, ym: TMountain) => bitAnd(y, x)
      case (xm: TFish, ym: THill) => bitAnd(y, x)
      case (xm: TFish, ym: TFish) => new TMountain(new BigDecimal(xm.getVal).toBigInteger and
          new BigDecimal(ym.getVal).toBigInteger)
      case (_, _) => new TError(1)
    }
  }
  def bitOr(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(bitOr(_, _), xl, yl)
      case (xl: LList, _) => mool(bitOr(_, _), xl, y)
      case (_, yl: LList) => bitOr(y, x)
      case (xm: TMountain, ym: TMountain) => new TMountain(xm.getVal or ym.getVal)
      case (xm: TMountain, ym: THill) => new TMountain(xm.getVal or BigInteger.valueOf(ym.getVal))
      case (xm: TMountain, ym: TFish) => new TMountain(xm.getVal or new BigDecimal(ym.getVal).toBigInteger)
      case (xm: THill, ym: TMountain) => bitOr(y, x)
      case (xm: THill, ym: THill) => new THill(xm.getVal | ym.getVal)
      case (xm: THill, ym: TFish) => new TMountain(BigInteger.valueOf(xm.getVal) or new BigDecimal(ym.getVal).toBigInteger)
      case (xm: TFish, ym: TMountain) => bitOr(y, x)
      case (xm: TFish, ym: THill) => bitOr(y, x)
      case (xm: TFish, ym: TFish) => new TMountain(new BigDecimal(xm.getVal).toBigInteger or
          new BigDecimal(ym.getVal).toBigInteger)
      case (_, _) => new TError(1)
    }
  }
  def bitXor(x: Type, y: Type): Type = {
    (x, y) match {
      case (xl: LList, yl: LList) => motl(bitXor(_, _), xl, yl)
      case (xl: LList, _) => mool(bitXor(_, _), xl, y)
      case (_, yl: LList) => bitXor(y, x)
      case (xm: TMountain, ym: TMountain) => new TMountain(xm.getVal xor ym.getVal)
      case (xm: TMountain, ym: THill) => new TMountain(xm.getVal xor BigInteger.valueOf(ym.getVal))
      case (xm: TMountain, ym: TFish) => new TMountain(xm.getVal xor new BigDecimal(ym.getVal).toBigInteger)
      case (xm: THill, ym: TMountain) => bitXor(y, x)
      case (xm: THill, ym: THill) => new THill(xm.getVal ^ ym.getVal)
      case (xm: THill, ym: TFish) => new TMountain(BigInteger.valueOf(xm.getVal) xor new BigDecimal(ym.getVal).toBigInteger)
      case (xm: TFish, ym: TMountain) => bitXor(y, x)
      case (xm: TFish, ym: THill) => bitXor(y, x)
      case (xm: TFish, ym: TFish) => new TMountain(new BigDecimal(xm.getVal).toBigInteger xor
          new BigDecimal(ym.getVal).toBigInteger)
      case (_, _) => new TError(1)
    }
  }
}
