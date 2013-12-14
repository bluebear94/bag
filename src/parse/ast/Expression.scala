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

class XprInt extends JavaTokenParsers with PackratParsers {
  // Regex for valid identifiers.
  def id: Parser[String] = """[$[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*:]?]?[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*|!]""".r // EEK
  // note: ! is allowed, just not at the beginning (otherwise it has to be the only character)
  trait Expression {
    def eval(ci: RunningInstance): Type
    // As you know, we are storing variables (including functions)
    // in intermediate bytecode (see specifications.txt).
    // def toByteCode: Array[Byte]
  }
  trait LValue extends Expression {
    def assign(ci: RunningInstance, t: Type): Unit
  }
  case class Literal(t: Type) extends Expression {
    def eval(ci: RunningInstance): Type = t
  }
  case class Variable(name: String) extends LValue {
    def eval(ci: RunningInstance): Type = ci.getVar(name)
    def assign(ci: RunningInstance, t: Type) = ci.setVar(name, t)
  }
  case class FCall(f: Type, args: Array[Expression]) extends Expression {
    def eval(ci: RunningInstance): Type = {
      if (f.getType == 7) f.asInstanceOf[TFunction](args.map(_.eval(ci)))
      else new TError(1)
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
              }
          )
      )
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
  lazy val void: Parser[Expression] = "Void" ^^^ {new Literal(new TVoid())}
  lazy val variable: Parser[Expression] = id ^^ {s => Variable(s)}
  lazy val mountain: Parser[Expression] = wholeNumber ^^ {s => new Literal(new TMountain(new BigInteger(s)))}
  lazy val hill: Parser[Expression] = """↼[-]?\d+""".r ^^ {s => new Literal(new THill(s.substring(1).toLong))}
  lazy val string: Parser[Expression] = stringLiteral ^^ {s => new Literal(new TString(s))}
  lazy val fish: Parser[Expression] = floatingPointNumber ^^ {s => new Literal(new TFish(s.toFloat))}
  lazy val literal: Parser[Expression] = void | mountain | hill | string | fish | array | linked
  lazy val commaDelimited: PackratParser[List[Expression]] = repsep(expression, ",")
  lazy val array: PackratParser[Expression] = "{" ~> commaDelimited <~ "}" ^^ {l => AList(true, l.toArray[Expression])}
  lazy val linked: PackratParser[Expression] = "[" ~> commaDelimited <~ "]" ^^ {l => AList(true, l.toArray[Expression])}
  lazy val expression: PackratParser[Expression] = literal | variable
  // Some tests :P
  class ExpressionParsersTest extends FlatSpec with ShouldMatchers {
    private def parsing[T](s:String)(implicit p:Parser[T]):T = {
      //wrap the parser in the phrase parse to make sure all input is consumed
      val phraseParser = phrase(p)
      //we need to wrap the string in a reader so our parser can digest it
      val input = new CharSequenceReader(s) 
      phraseParser(input) match {
          case Success(t,_)     => t
          case NoSuccess(msg,_) => throw new IllegalArgumentException(
                                       "Could not parse '" + s + "': " + msg)
      }
    }
    private def assertFail[T](input:String)(implicit p:Parser[T]) {
      evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
    }
    "XprInt" should "parse void literals" in {
      implicit val parserToTest = void
      parsing("Void") should equal (Literal(new TVoid))
      assertFail("void")
    }
    "XprInt" should "parse mountains" in {
      implicit val parserToTest = mountain
      parsing("29") should equal (Literal(TMountain(new BigInteger("29"))))
      parsing("533") should equal (Literal(TMountain(new BigInteger("533"))))
      parsing("533644755866977533644755866977") should equal (Literal(TMountain(new BigInteger("533644755866977533644755866977"))))
      parsing("-2") should equal (Literal(TMountain(new BigInteger("-2"))))
    }
    "XprInt" should "parse hills" in {
      implicit val parserToTest = hill
      parsing("↼90") should equal (Literal(THill(90L)))
      parsing("↼95134") should equal (Literal(THill(95134L)))
      parsing("↼-1") should equal (Literal(THill(-1L)))
    }
    "XprInt" should "parse strings" in {
      implicit val parserToTest = string
      parsing("\"Nadenva\"") should equal (Literal(new TString("Nadenva")))
      parsing("\"\\\"Nadenva\"") should equal (Literal(new TString("\"Nadenva")))
    }
    "XprInt" should "parse fish" in {
      implicit val parserToTest = fish
      parsing("3.0") should equal (Literal(TFish(3.0)))
      parsing("3.") should equal (Literal(TFish(3.0)))
      parsing("15.6") should equal (Literal(TFish(15.6)))
      parsing("-1.98") should equal (Literal(TFish(-1.98)))
      parsing("0.9") should equal (Literal(TFish(0.9)))
      parsing(".5") should equal (Literal(TFish(.5)))
    }
    "XprInt" should "parse arrays and linked lists" in {
      implicit val parserToTest = literal
      parsing("{3, 4, 5}") should equal (Literal(new LArray(
          (Array[Type](
        		  TMountain(new BigInteger("3")),
        		  TMountain(new BigInteger("4")),
        		  TMountain(new BigInteger("5")))).to[ArrayBuffer])))
      parsing("[3, 4, 5]") should equal (Literal(new LLinked(
          (Array[Type](
        		  TMountain(new BigInteger("3")),
        		  TMountain(new BigInteger("4")),
        		  TMountain(new BigInteger("5")))).to[ListBuffer])))
    }
    "XprInt" should "use operators" in {
      implicit val parserToTest = expression // TODO: change the type of parser
      parsing("4 + 9") should equal (Operator("+", Literal(TMountain(new BigInteger("4"))), Literal(TMountain(new BigInteger("9")))))
      assertFail("4 +")
      parsing("7 - 3") should equal (Operator("-", Literal(TMountain(new BigInteger("7"))), Literal(TMountain(new BigInteger("3")))))
      parsing("5 * 8") should equal (Operator("*", Literal(TMountain(new BigInteger("5"))), Literal(TMountain(new BigInteger("8")))))
    }
  }
}


