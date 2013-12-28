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
import java.util.TreeMap // darn, no mutable TreeMap yet

trait Expression {
  def eval(ci: RunningInstance): Type
  // As you know, we are storing variables (including functions)
  // in intermediate bytecode (see specifications.txt).
  // def toByteCode: Array[Byte]
  // def toString: String
}
trait SBExpression extends Expression
trait LValue extends SBExpression {
  def assign(ci: RunningInstance, t: Type): Unit
  // def nuke(ci: RunningInstance): Unit
}
case class Literal(t: Type) extends SBExpression {
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
case class FCall(f: SBExpression, args: Array[Expression]) extends SBExpression {
  def eval(ci: RunningInstance): Type = {
    val g = f.eval(ci)
    if (g.getType == 7) g.asInstanceOf[TFunction](args.map(_.eval(ci)))
    else new TError(1)
  }
}
case class Lambda(lines: List[Expression]) extends SBExpression {
  def eval(ci: RunningInstance): Type = {
    new TASTFunc(lines, ci)
  }
}
case class AList(isArray: Boolean, args: Array[Expression]) extends SBExpression {
  def eval(ci: RunningInstance): Type = {
    if (isArray)
      new LArray(args.map(_.eval(ci)).to[ArrayBuffer])
    else
      new LLinked(args.map(_.eval(ci)).to[ListBuffer])
  }
}
case class Index(l: Expression, i: Expression) extends SBExpression {
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
case class SBWrapper(x: Expression) extends SBExpression {
  def eval(ci: RunningInstance): Type = x.eval(ci)
}
class XprInt extends JavaTokenParsers with PackratParsers {
  var ops: TreeMap[Int, Parser[(Expression, Expression) => Expression]] = new TreeMap[Int, Parser[(Expression, Expression) => Expression]]()
  // Regex for valid identifiers.
  //[$[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*:]?]?
  def id: Regex = """[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",<>/?]*|!]""".r // EEK
  // note: ! is allowed, just not at the beginning (otherwise it has to be the only character)
  lazy val void: Parser[SBExpression] = "Void" ^^^ { new Literal(new TVoid()) }
  lazy val variable: Parser[SBExpression] = ("$".r | "".r | ("$".r ~ id ~ ":".r)) ~ id ^^ { s => Variable(s._1 + s._2) }
  lazy val mountain: Parser[SBExpression] = wholeNumber ^^ { s => new Literal(new TMountain(new BigInteger(s))) }
  lazy val hill: Parser[SBExpression] = """↼[-]?\d+""".r ^^ { s => new Literal(new THill(s.substring(1).toLong)) }
  lazy val string: Parser[SBExpression] = stringLiteral ^^ { s => new Literal(new TString(s)) }
  lazy val fish: Parser[SBExpression] = floatingPointNumber ^^ { s => new Literal(new TFish(s.toDouble)) }
  lazy val literal: Parser[SBExpression] = void | mountain | hill | string | fish
  val lineDelimiter: Parser[String] = ";" | "\n"
  lazy val commaDelimited: PackratParser[List[Expression]] = repsep(expression, ",")
  lazy val lineDelimited: PackratParser[List[Expression]] = repsep(expression, lineDelimiter)
  lazy val array: PackratParser[SBExpression] = "{" ~> commaDelimited <~ "}" ^^ { l => AList(true, l.toArray[Expression]) }
  lazy val linked: PackratParser[SBExpression] = "[" ~> commaDelimited <~ "]" ^^ { l => AList(true, l.toArray[Expression]) }
  lazy val hashtag: PackratParser[SBExpression] = "#" ~> sbexpression ^^ { x => Hashtag(x) }
  lazy val lambda: PackratParser[SBExpression] = "λ" ~> lineDelimited <~ "Endλ" ^^ { l => Lambda(l) }
  lazy val call: PackratParser[Expression] = sbexpression ~ "(" ~ commaDelimited <~ ")" ^^ { sh => FCall(sh._1._1, sh._2.toArray[Expression]) }
  lazy val ifst: PackratParser[Expression] = "If " ~> expression ~ lineDelimiter ~ expression ^^ { sh => If(sh._1._1, sh._2) }
  lazy val ifThen: PackratParser[Expression] = "If " ~> expression ~ lineDelimiter ~ "Then" ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndIf" ^^
    { sh => IfThen(sh._1._1._1._1, sh._2) }
  lazy val ifThenElse: PackratParser[Expression] = ("If " ~> expression <~ lineDelimiter) ~ ("Then" ~> lineDelimiter ~> lineDelimited <~
    lineDelimiter) ~ ("Else" ~> lineDelimiter ~> lineDelimited <~ lineDelimiter <~ "EndIf") ^^
    { case (condExpr ~ thens ~ elses) => IfThenElse(condExpr, thens, elses) }
  lazy val forst: PackratParser[Expression] = ("For " ~> commaDelimited <~ lineDelimiter) ~ lineDelimited <~ (lineDelimiter ~ "EndFor") ^^ {
    case (f3 ~ body) => f3(0) match {
      case lv: LValue => For(lv, f3(1), f3(2), if (f3.length == 3) Literal(THill(1L)) else f3(3), body)
    }
  }
  lazy val whilst: PackratParser[Expression] = "While " ~> expression ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndWhile" ^^ { sh =>
    While(sh._1._1, sh._2)
  }
  lazy val repeat: PackratParser[Expression] = "Repeat " ~> expression ~ lineDelimiter ~ lineDelimited <~ lineDelimiter ~ "EndRept" ^^ { sh =>
    Repeat(sh._1._1, sh._2)
  }
  lazy val indexing: PackratParser[SBExpression] = sbexpression ~ "[" ~ expression <~ "]" ^^
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
  lazy val compound: PackratParser[SBExpression] = array | linked | hashtag | lambda | indexing
  lazy val expression: PackratParser[Expression] = operator(ops.firstKey) | sbexpression | control
  lazy val sbwrapper: PackratParser[SBExpression] = "(" ~> expression <~ ")" ^^ { x => SBWrapper(x) }
  lazy val sbexpression: PackratParser[SBExpression] = literal | variable | compound | sbwrapper
  def loadOps = {
    val ll = Global.liblist.keySet.toList
    print(s"Loading operators from libraries: $ll\n")
    for (ln <- ll) { // loop over every library loaded
      print(s"Loading operators from $ln\n")
      val cl = Global.liblist(ln)
      val col = cl.ccol
      println("Operators: "+ col.opList.keySet.toList)
      val isStdLib = ln == "std"
      for (cmdsp <- col.opList.toList) {
        val cmd = cmdsp._2
        if (!cmd.isUnary) {
          val prec = cmd.getPrecedence
          val dir = cmd.isReversed
          val opn = (if (isStdLib) "" else "$" + ln + ":") + cmd.getOpAlias
          print(s"Loading operator $opn\n")
          if (ops.containsKey(prec)) {
            // update parser
            val oldp = ops.get(prec)
            // create a parser
            val cp: Parser[(Expression, Expression) => Expression] = opn ^^^ { (a, b) =>
              if (dir) Operator(opn, b, a)
              else Operator(opn, a, b)
            }
            // now update the map with the new parser combined
            ops.put(prec, oldp | cp)
          } else {
            // create a new entry in the map
            val cp: Parser[(Expression, Expression) => Expression] = opn ^^^ { (a, b) =>
              if (dir) Operator(opn, b, a)
              else Operator(opn, a, b)
            }
            ops.put(prec, cp)
          }
        }
      }
    }
  }
  def operator(level: Int): Parser[Expression] = {
    if (level >= ops.lastKey) sbexpression
    else operator(ops.ceilingKey(level)) * ops.get(level)
  }
}


