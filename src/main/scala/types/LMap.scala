package types

import scala.collection.immutable.Set
import scala.collection.mutable._
import util.MakeByteArrays
import cmdreader.Global

class LMap(h: HashMap[Type, Type]) extends FuncLike {
  def getType(): Int = 8
  def gm() = h
  def toBoolean(): Boolean = h.isEmpty
  def equals(that: Type) = {
    (that.getType == 8) && h == that.asInstanceOf[LMap].gm
  }
  override def hashCode: Int = {
    h.foldLeft(83)((a, b) => 97 * a + 41 * b._1.hashCode + 101 * b._2.hashCode)
  }
  def toStringNC(): String = "Map(" + h.toList.map(p => (p._1.toStringNC + " → " + p._2.toStringNC)).mkString(", ") + ")"
  def toStringC_(visited: Set[Type]): String = "Map(" + (h.toList map { case (k, v) => 
    k.toStringC(visited + this) + " → " + v.toStringC(visited + this) } mkString(", ")) + ")"
  def lu(i: Type, n: Type) = {
    h(i) = n
  }
  def du(i: Type) = {
    h.remove(i)
  }
  def >/< = new LMap(h.map {case (k, v) => (k.>/<, v.>/<)})
  def toBytecode: Array[Byte] = {
    val s = h.toList.map((p: (Type, Type)) => {
      val t = p._1
      val u = p._2
      val bc = t.toBytecode
      val cc = u.toBytecode
      MakeByteArrays.intToByteArray(bc.length) ++ Array[Byte](t.getType.toByte) ++ bc ++
        MakeByteArrays.intToByteArray(cc.length) ++ Array[Byte](u.getType.toByte) ++ cc
    }).foldLeft(Array[Byte]())(_ ++ _)
    val bsl = h.toList.length
    MakeByteArrays.intToByteArray(bsl) ++ s
  }
  def apply(args: Array[Type]) = {
    val env = gm.map { case (k, v) => (k.toString, v) }
    env("this") match {
      case f: TFunction => f.applyWith(if (Global.vigilant) args.map(_.>/<) else args, env)
      case _ => new TError(1)
    }
  }
  override def equals(that: Any) = {
    that match {
      case other: LMap => gm.equals(other.gm)
      case _ => false
    }
  }
  def equalsStrictly(that: Type) = {
    that match {
      case t: LMap => h == t.gm
      case _ => false
    }
  }
  private def toList = {
    h.toList.map((p: (Type, Type)) => {
      List(p._1, p._2)
    }).reduceLeft(_ ++ _)
  }
  def cast(i: Int): Type = i match {
    case 8 => this
    case 0 => TVoid.inst
    case 5 => new LArray(toList.to[ArrayBuffer])
    case 6 => new LLinked(toList.to[ListBuffer])
  }
}
