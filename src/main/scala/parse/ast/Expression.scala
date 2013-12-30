package parse.ast

import types._
import run.RunningInstance
import scala.util.parsing.combinator._
import java.math.BigInteger
import types._
import org.scalatest._
import scala.util.parsing.input.CharSequenceReader
import cmdreader._
import scala.collection.mutable
import util._
import scala.util.matching.Regex
import java.util.TreeMap // darn, no mutable TreeMap yet

trait Expression {
  def eval(ci: RunningInstance): Type
  // As you know, we are storing variables (including functions)
  // in intermediate bytecode (see specifications.txt).
  def toBytecode: Array[Bin] // this is going to take FOREVER
  // def toString: String
}
trait SBExpression extends Expression
trait LValue extends SBExpression {
  def assign(ci: RunningInstance, t: Type): Unit
  // def nuke(ci: RunningInstance): Unit
  def toSymBytecode: Array[Bin]
}
case class Literal(t: Type) extends SBExpression {
  def eval(ci: RunningInstance): Type = t
  def toBytecode = Array(Bytes(Array[Byte](-0x1F, t.getType.toByte) ++ t.toBytecode))
}
case class Variable(name: String) extends LValue {
  def eval(ci: RunningInstance): Type = ci.getVar(name)
  def assign(ci: RunningInstance, t: Type) = ci.setVar(name, t)
  def toBytecode = {
    val inB = name.getBytes
    Array(Bytes(Array[Byte](-0x20, 0x04) ++ MakeByteArrays.intToByteArray(inB.length) ++ inB))
  }
  def toSymBytecode = {
    val inB = name.getBytes
    Array(Bytes(Array[Byte](-0x20, 0x05) ++ MakeByteArrays.intToByteArray(inB.length) ++ inB))
  }
}
case class Assign(left: LValue, right: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    left.assign(ci, right.eval(ci).>/<)
    left.eval(ci)
  }
}
case class AssignOp(left: LValue, right: Expression, op: String) extends Expression {
  def eval(ci: RunningInstance): Type = {
    left.assign(ci, Operator(op, left, right).eval(ci))
    left.eval(ci)
  }
}
case class DoubleOp(left: LValue, op: String, post: Boolean) extends SBExpression {
  def eval(ci: RunningInstance): Type = {
    val oldV = left.eval(ci).>/<
    val db = Global.getCmd(op).getDoubleBase match {
      case Some(v) => v
      case None => throw new UnsupportedOperationException
    }
    left.assign(ci, Operator(op, left, Literal(db)).eval(ci))
    if (post) oldV else left.eval(ci)
  }
}
case class FCall(f: SBExpression, args: Array[Expression]) extends SBExpression {
  def eval(ci: RunningInstance): Type = {
    val g = f.eval(ci)
    g match {
      case h: TFunction => h(args.map(_.eval(ci)))
      case _ => new TError(1)
    }
  }
  def toBytecode = {
    BFuncs.app(args.map(_.toBytecode).foldLeft(Array[Bin]())(_ ++ _),
      f.toBytecode) ++
      Array(Bytes(Array[Byte](-0x20, 0x00) ++ MakeByteArrays.intToByteArray(args.length)))
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
      new LArray(args.map(_.eval(ci)).to[mutable.ArrayBuffer])
    else
      new LLinked(args.map(_.eval(ci)).to[mutable.ListBuffer])
  }
  def toBytecode = {
    args.map(_.toBytecode).foldLeft(Array[Bin]())(_ ++ _) ++
      Array(Bytes(Array[Byte](-0x15, if (isArray) 0x40 else 0x50)))
  }
}
case class Index(l: Expression, i: Expression) extends SBExpression {
  def eval(ci: RunningInstance): Type = {
    Indexing.index(l.eval(ci), i.eval(ci))
  }
  def toBytecode = {
    BFuncs.app(l.toBytecode,
      i.toBytecode) ++
      Array(Bytes(Array[Byte](-0x15, 0x39)))
  }
}
case class LIndex(l: LValue, i: Expression) extends LValue {
  def eval(ci: RunningInstance): Type = {
    Indexing.index(l.eval(ci), i.eval(ci))
  }
  def assign(ci: RunningInstance, n: Type) = {
    var t: Type = l.eval(ci)
    Indexing.setIndex(t, i.eval(ci), n)
    l.assign(ci, t)
  }
  def toBytecode = {
    BFuncs.app(l.toBytecode,
      i.toBytecode) ++
      Array(Bytes(Array[Byte](-0x15, 0x39)))
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
  def toBytecode = {
    val name2 = getCmd.getName
    val col = name.indexOf(":")
    val lib = if (col == -1) "" else {
      name.substring(1, col - 1)
    }
    FCall(Variable("$" + lib + ":" + name2), Array(l, r)).toBytecode
  }
}
case class Ternary(p: Expression, t: Expression, f: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean)
      t.eval(ci)
    else
      f.eval(ci)
  }
  def toBytecode = {
    val predicate = p.toBytecode
    val trueBody = t.toBytecode
    val falseBody = f.toBytecode
    BFuncs.app(predicate, BFuncs.app(
      Array(Bytes(Array[Byte](-0x15, 0x33)), Offset(6 + BFuncs.alen(falseBody))),
      BFuncs.app(falseBody, trueBody)))
  }
}
case class If(p: Expression, t: Expression) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean) {
      t.eval(ci)
    }
    new TVoid
  }
  def toBytecode = {
    val predicate = p.toBytecode
    val trueBody = t.toBytecode
    BFuncs.app(predicate, BFuncs.app(
      Array(Bytes(Array[Byte](-0x15, 0x23)), Offset(6 + BFuncs.alen(trueBody))),
      trueBody))
  }
}
case class IfThen(p: Expression, t: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    if (p.eval(ci).toBoolean) {
      t.map(_.eval(ci))
    }
    new TVoid
  }
  def toBytecode = {
    val predicate = p.toBytecode
    val trueBody = t.map(_.toBytecode).foldLeft(Array[Bin]())(BFuncs.app(_, _))
    BFuncs.app(predicate, BFuncs.app(
      Array(Bytes(Array[Byte](-0x15, 0x23)), Offset(6 + BFuncs.alen(trueBody))),
      trueBody))
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
  def toBytecode = {
    val predicate = p.toBytecode
    val trueBody = t.map(_.toBytecode).foldLeft(Array[Bin]())(BFuncs.app(_, _))
    val falseBody = f.map(_.toBytecode).foldLeft(Array[Bin]())(BFuncs.app(_, _))
    BFuncs.app(predicate, BFuncs.app(
      Array(Bytes(Array[Byte](-0x15, 0x33)), Offset(6 + BFuncs.alen(falseBody))),
      BFuncs.app(falseBody, trueBody)))
  }
}
case class While(p: Expression, b: List[Expression]) extends Expression {
  def eval(ci: RunningInstance): Type = {
    while (p.eval(ci).toBoolean) {
      b.map(_.eval(ci))
    }
    new TVoid
  }
  def toBytecode = {
    val predicate = p.toBytecode
    val body = b.map(_.toBytecode).foldLeft(Array[Bin]())(BFuncs.app(_, _))
    val bl = BFuncs.alen(body)
    BFuncs.app(predicate, BFuncs.app(
      Array(Bytes(Array[Byte](-0x15, 0x32)), Offset(12 + bl)), BFuncs.app(
        body, Array(Bytes(Array[Byte](-0x15, 0x34)), Offset(-bl - 6 - BFuncs.alen(predicate))))))
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
  def toBytecode = x.toBytecode
}
class XprInt extends JavaTokenParsers with PackratParsers {
  var ops: TreeMap[Int, PackratParser[(Expression, Expression) => Expression]] =
    new TreeMap[Int, PackratParser[(Expression, Expression) => Expression]]()
  val oeOps: mutable.ListBuffer[String] = new mutable.ListBuffer[String]()
  val ooOps: mutable.ListBuffer[String] = new mutable.ListBuffer[String]()
  var opEq: PackratParser[Expression] = failure("No such assignment operator.")
  // Regex for valid identifiers.
  //[$[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*:]?]?
  def id: Regex = """[^\x5C\s\Q^!@#$%^&*_-+~{}().[]=|:;'",<>/?\E][^\x5C\s\Q^@#$%^&*_-+~{}()[]=|:;'",<>/?\E]*|!""".r // EEK
  // note: ! is allowed, just not at the beginning (otherwise it has to be the only character)
  lazy val void: PackratParser[SBExpression] = "Void" ^^^ { new Literal(new TVoid()) }
  lazy val variable: PackratParser[LValue] = ((("\\$".r ~ (id | "") ~ ":") ^^ {
    case d ~ lib ~ c => "$" + lib + ":"
  } | "\\$".r | "".r) ~ id)
    .filter(w => !Keywords.keywords.contains(w._2)) ^^
    { s => Variable(s._1 + s._2) }
  lazy val mountain: PackratParser[SBExpression] = wholeNumber ^^ { s => new Literal(new TMountain(new BigInteger(s))) }
  lazy val hill: PackratParser[SBExpression] = """↼[-]?\d+""".r ^^ { s => new Literal(new THill(s.substring(1).toLong)) }
  lazy val string: PackratParser[SBExpression] = stringLiteral ^^ { s =>
    UnescapeString.unescape(s.substring(1, s.length - 1)) match {
      case Some(ues) => Literal(new TString(ues))
      case None => Literal(new TError(7))
    }
  }
  lazy val fish: PackratParser[SBExpression] = floatingPointNumber ^^ { s => new Literal(new TFish(s.toDouble)) }
  lazy val literal: PackratParser[SBExpression] = void | fish ||| mountain | hill | string
  val lineDelimiter: PackratParser[String] = ";" | "\n"
  lazy val commaDelimited: PackratParser[List[Expression]] = repsep(expression, ",")
  lazy val lineDelimited: PackratParser[List[Expression]] = repsep(expression, lineDelimiter)
  lazy val array: PackratParser[SBExpression] = "{" ~> commaDelimited <~ "}" ^^ { l => AList(true, l.toArray[Expression]) }
  lazy val linked: PackratParser[SBExpression] = "[" ~> commaDelimited <~ "]" ^^ { l => AList(false, l.toArray[Expression]) }
  lazy val hashtag: PackratParser[LValue] = "#" ~> sbexpression ^^ { x => Hashtag(x) }
  lazy val lambda: PackratParser[SBExpression] = ("λ" ~ lineDelimiter) ~> lineDelimited <~ (lineDelimiter ~ "Endλ") ^^ { l => Lambda(l) }
  lazy val call: PackratParser[SBExpression] = sbexpression ~ "(" ~ commaDelimited <~ ")" ^^ { sh => FCall(sh._1._1, sh._2.toArray[Expression]) }
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
        Index(thing, index)
      }
    }
  lazy val lIndexing: PackratParser[LValue] = lvalue ~ "[" ~ expression <~ "]" ^^ {
    sh =>
      {
        val thing = sh._1._1
        val index = sh._2
        LIndex(thing, index)
      }
  }
  lazy val control: PackratParser[Expression] = ifst | ifThen | ifThenElse | forst | whilst | repeat
  def lvalue: PackratParser[LValue] = lIndexing | hashtag | variable
  lazy val assign: PackratParser[Expression] = lvalue ~ "=" ~ expression ^^ {
    case (left ~ o ~ right) =>
      Assign(left, right)
  }
  //lazy val assignOp: PackratParser[Expression]
  lazy val compound: PackratParser[SBExpression] = lambda | lIndexing | indexing | array | linked | hashtag | call
  lazy val ternary: PackratParser[Expression] = sbexpression ~ "?" ~ expression ~ ":" ~ expression ^^ {
    case p ~ "?" ~ t ~ ":" ~ f => Ternary(p, t, f)
  }
  def expression: PackratParser[Expression] = control | ternary | getOpEq | assign | operator(ops.firstKey) | sbexpression
  def sbwrapper: PackratParser[SBExpression] = "(" ~> expression <~ ")" ^^ { x => SBWrapper(x) }
  def sbexpression: PackratParser[SBExpression] = sbwrapper | literal | compound | (getLOpOp ||| getOpOpL) | variable
  def getOpEq: PackratParser[Expression] = {
    if (oeOps.isEmpty) failure("no such operator")
    else lvalue ~ (oeOps.tail.foldLeft(literal(oeOps.head))((p, op) => p | op)) ~ "=" ~ expression ^^ {
      case left ~ op ~ "=" ~ right => AssignOp(left, right, op)
    }
  }
  def getOpOpL: PackratParser[SBExpression] = {
    if (ooOps.isEmpty) failure("no such operator")
    else {
      (ooOps.tail.foldLeft(literal(ooOps.head))((p, op) => p | op)) ~ lvalue ^^ {
        case op ~ l => DoubleOp(l, op.substring(op.length / 2), false)
      }
    }
  }
  def getLOpOp: PackratParser[SBExpression] = {
    if (ooOps.isEmpty) failure("no such operator")
    else {
      lvalue ~ (ooOps.tail.foldLeft(literal(ooOps.head))((p, op) => p | op)) ^^ {
        case l ~ op => DoubleOp(l, op.substring(op.length / 2), true)
      }
    }
  }
  def loadWithPrec(prec: Int, parser: PackratParser[(Expression, Expression) => Expression]) = {
    if (ops.containsKey(prec)) {
      // update parser
      val oldp = ops.get(prec)
      ops.put(prec, oldp | parser)
    } else {
      // create a new entry in the map
      ops.put(prec, parser)
    }
  }
  def loadOps = { // Long-ass method to use necessary information about operators to build parsers for them
    val ll = Global.liblist.keySet.toList
    print(s"Loading operators from libraries: $ll\n")
    for (ln <- ll) { // loop over every library loaded
      print(s"Loading operators from $ln\n")
      val cl = Global.liblist(ln)
      val col = cl.ccol
      println("Operators: " + col.opList.keySet.toList)
      val isStdLib = ln == "std"
      for (cmdsp <- col.opList.toList) {
        val cmd = cmdsp._2
        if (!cmd.isUnary) {
          val prec = cmd.getPrecedence
          val dir = cmd.isReversed
          val opn = (if (isStdLib) "" else "$" + ln + ":") + cmd.getOpAlias
          val hasOE = cmd.hasAssignmentEquiv
          print(s"Loading operator $opn\n")
          val cp: PackratParser[(Expression, Expression) => Expression] = opn ^^^ { (a: Expression, b: Expression) =>
            if (dir) Operator(opn, b, a)
            else Operator(opn, a, b)
          }
          loadWithPrec(prec, cp)
          // now update the opEq parser, if appropriate
          if (hasOE) {
            println(s"Loading variation $opn=")
            oeOps += opn
            //opEq = lvalue ~ opn ~ "=" ~ expression ^^ {
            //  case (left ~ o ~ "=" ~ right) => AssignOp(left, right, o)
            //} | getOpEq
          }
          cmd.getDoubleBase match {
            case Some(t) => {
              ooOps += opn + opn
              println(s"Loading variation $opn$opn")
            }
            case None => ()
          }
        }
      }
    }
    // now add the logical and and or parsers (short-circuit)
    val andParser: PackratParser[(Expression, Expression) => Expression] = "&&" ^^^
      { (a: Expression, b: Expression) => Ternary(a, b, Literal(TMountain(BigInteger.ZERO))) }
    val orParser: PackratParser[(Expression, Expression) => Expression] = "||" ^^^
      { (a: Expression, b: Expression) => Ternary(a, Literal(TMountain(BigInteger.ONE)), b) }
    loadWithPrec(PStandard.CONJUNCTION, andParser)
    loadWithPrec(PStandard.DISJUNCTION, orParser)
  }
  def operator(level: Int): PackratParser[Expression] = {
    if (level > ops.lastKey) sbexpression
    else if (level == ops.lastKey) sbexpression * ops.get(level)
    else operator(ops.higherKey(level)) * ops.get(level)
  }
}


