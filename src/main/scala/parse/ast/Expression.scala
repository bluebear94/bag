package parse.ast

import types._
import run.RunningInstance
import scala.util.parsing.combinator._
import java.math.BigInteger
import types._
import org.scalatest._
import scala.util.parsing.input.CharSequenceReader
import cmdreader._
import scala.collection.mutable._
import util._
import scala.util.matching.Regex

trait Expression {
  def eval(ci: RunningInstance): Type
  // As you know, we are storing variables (including functions)
  // in intermediate bytecode (see specifications.txt).
  // def toByteCode: Array[Byte]
  // def toString: String
}
trait LValue extends Expression {
  def assign(ci: RunningInstance, t: Type): Unit
  // def nuke(ci: RunningInstance): Unit
}
case class Literal(t: Type) extends Expression {
  def eval(ci: RunningInstance): Type = t
}
case class Variable(name: String) extends LValue {
  def eval(ci: RunningInstance): Type = ci.getVar(name)
  def assign(ci: RunningInstance, t: Type) = ci.setVar(name, t)
}
case class Assign(left: LValue, right: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    left.assign(ci, right.eval(ci))
    left.eval(ci)
  }
}
case class AssignOp(left: LValue, right: Expression, op: String) extends Expression {
  def eval(ci: RunningInstance): Type = {
    left.assign(ci, Operator(op, left, right).eval(ci))
    left.eval(ci)
  }
}
case class FCall(f: Expression, args: Array[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    val g = f.eval(ci)
    if (g.getType == 7) g.asInstanceOf[TFunction](args.map(_.eval(ci)))
    else new TError(1)
  }
}
case class Lambda(lines: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    new TASTFunc(lines, ci)
  }
}
case class AList(isArray: Boolean, args: Array[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (isArray)
      new LArray(args.map(_.eval(ci)).to[ArrayBuffer])
    else
      new LLinked(args.map(_.eval(ci)).to[ListBuffer])
  }
}
case class Index(l: Expression, i: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    Indexing.index(l.eval(ci), i.eval(ci))
  }
}
case class LIndex(l: LValue, i: Expression) extends LValue {
  def eval(ci: RunningInstance): Type = {
    Indexing.index(l.eval(ci), i.eval(ci))
  }
  def assign(ci: RunningInstance, n: Type) = {
    val t: Type = l.eval(ci)
    Indexing.setIndex(t, i.eval(ci), n)
  }
}
// How am I going to parse operations while respecting the order of
// operations given by the getPrecedence method in the
// CommandOperator class?
case class Operator(name: String, l: Expression, r: Expression) extends Expression {
  def getCmd(): CommandOperator = {
    Global.getCmd(name)
  }
  def eval(ci: RunningInstance): Type = {
    getCmd()(Array[Type](
      l match {
        case Literal(t) => t
        case _ => l.eval(ci)
      },
      r match {
        case Literal(t) => t
        case _ => r.eval(ci)
      }))
  }
  def getPrec: Int = {
    getCmd().getPrecedence
  }
}
case class Ternary(p: Expression, t: Expression, f: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean)
      t.eval(ci)
    else
      f.eval(ci)
  }
}
case class If(p: Expression, t: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean) {
      t.eval(ci)
    }
    new TVoid
  }
}
case class IfThen(p: Expression, t: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean) {
      t.map(_.eval(ci))
    }
    new TVoid
  }
}
case class IfThenElse(p: Expression, t: List[Expression], f: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean) {
      t.map(_.eval(ci))
    } else {
      f.map(_.eval(ci))
    }
    new TVoid
  }
}
case class While(p: Expression, b: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    while (p.eval(ci).toBoolean) {
      b.map(_.eval(ci))
    }
    new TVoid
  }
}
case class Repeat(p: Expression, b: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    do {
      b.map(_.eval(ci))
    } while (!p.eval(ci).toBoolean)
    new TVoid
  }
}
case class For(v: LValue, st: Expression, end: Expression, inc: Expression, b: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    v.assign(ci, st.eval(ci))
    while (!v.eval(ci).gt(end.eval(ci))) {
      b.map(_.eval(ci))
      v.assign(ci, MathUtil.add(v.eval(ci), inc.eval(ci)))
    }
    new TVoid
  }
}
case class Hashtag(x: Expression) extends LValue {
  def eval(ci: RunningInstance): Type = {
    val t = x.eval(ci)
    t match {
      case TMountain(n) => ci.argn(n.intValue)
      case THill(n) => ci.argn(n.asInstanceOf[Int])
      case TFish(n) => ci.argn(n.asInstanceOf[Int])
      case TString(n) => ci.getVar(n)
      case _ => new TError(1)
    }
  }
  def assign(ci: RunningInstance, t: Type): Unit = {
    val t2 = x.eval(ci)
    t2 match {
      case TMountain(n) => ci.setargn(n.intValue, t)
      case THill(n) => ci.setargn(n.asInstanceOf[Int], t)
      case TFish(n) => ci.setargn(n.asInstanceOf[Int], t)
      case TString(n) => ci.setVar(n, t)
      case _ => new TError(1)
    }
  }
}
class XprInt extends JavaTokenParsers with PackratParsers {
  // Regex for valid identifiers.
  //[$[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*:]?]?
  def id: Regex = """[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",<>/?]*|!]""".r // EEK
  // note: ! is allowed, just not at the beginning (otherwise it has to be the only character)
  lazy val void: Parser[Expression] = "Void" ^^^ { new Literal(new TVoid()) }
  lazy val variable: Parser[Expression] = ("$".r | "".r | ("$".r ~ id ~ ":".r)) ~ id ^^ { s => Variable(s._1 + s._2) }
  lazy val mountain: Parser[Expression] = wholeNumber ^^ { s => new Literal(new TMountain(new BigInteger(s))) }
  lazy val hill: Parser[Expression] = """↼[-]?\d+""".r ^^ { s => new Literal(new THill(s.substring(1).toLong)) }
  lazy val string: Parser[Expression] = stringLiteral ^^ { s => new Literal(new TString(s)) }
  lazy val fish: Parser[Expression] = floatingPointNumber ^^ { s => new Literal(new TFish(s.toFloat)) }
  lazy val literal: Parser[Expression] = void | mountain | hill | string | fish
  val lineDelimiter: Parser[String] = ";" | "\n"
  lazy val commaDelimited: PackratParser[List[Expression]] = repsep(expression, ",")
  lazy val lineDelimited: PackratParser[List[Expression]] = repsep(expression, lineDelimiter)
  lazy val array: PackratParser[Expression] = "{" ~> commaDelimited <~ "}" ^^ { l => AList(true, l.toArray[Expression]) }
  lazy val linked: PackratParser[Expression] = "[" ~> commaDelimited <~ "]" ^^ { l => AList(true, l.toArray[Expression]) }
  lazy val hashtag: PackratParser[Expression] = "#" ~> expression ^^ { x => Hashtag(x) }
  lazy val lambda: PackratParser[Expression] = "λ" ~> lineDelimited <~ "Endλ" ^^ { l => Lambda(l) }
  lazy val call: PackratParser[Expression] = variable ~ "(" ~ commaDelimited <~ ")" ^^ { sh => FCall(sh._1._1, sh._2.toArray[Expression]) }
  lazy val ifst: PackratParser[Expression] = "If " ~> expression ~ lineDelimiter ~ expression ^^ { sh => If(sh._1._1, sh._2) }
  lazy val ifThen: PackratParser[Expression] = "If " ~> expression ~ lineDelimiter ~ "Then" ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndIf" ^^
    { sh => IfThen(sh._1._1._1._1, sh._2) }
  lazy val ifThenElse: PackratParser[Expression] = "If " ~> expression ~ lineDelimiter ~ "Then" ~ lineDelimiter ~ lineDelimited ~
    lineDelimiter ~ "Else" ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndIf" ^^ { sh =>
      sh match {
        case e ~ _ ~ _ ~ _ ~ body ~ _ ~ _ ~ _ ~ body2 => IfThenElse(e, body, body2)
      }
    } // what the fuck
  lazy val forst: PackratParser[Expression] = "For " ~> commaDelimited ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndFor" ^^ { sh =>
    {
      val top = sh._1._1
      For(top(0).asInstanceOf[LValue], top(1), top(2), if (top.length == 3) Literal(THill(1L)) else top(3), sh._2)
    }
  }
  lazy val whilst: PackratParser[Expression] = "While " ~> expression ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndWhile" ^^ { sh =>
    While(sh._1._1, sh._2)
  }
  lazy val repeat: PackratParser[Expression] = "Repeat " ~> expression ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndRept" ^^ { sh =>
    Repeat(sh._1._1, sh._2)
  }
  lazy val indexing: PackratParser[Expression] = expression ~ "[" ~ expression <~ "]" ^^
    { sh =>
      {
        val thing = sh._1._1
        val index = sh._2
        if (thing.isInstanceOf[LValue]) LIndex(thing.asInstanceOf[LValue], index)
        else Index(thing, index)
      }
    }
  lazy val control: PackratParser[Expression] = ifst | ifThen | ifThenElse | forst | whilst | repeat
  //lazy val assign: PackratParser[Expression]
  //lazy val assignOp: PackratParser[Expression]
  lazy val compound: PackratParser[Expression] = array | linked | hashtag | lambda | indexing
  lazy val expression: PackratParser[Expression] = literal | variable | compound | control
  // Some tests :P
}


