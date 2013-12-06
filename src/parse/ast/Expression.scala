package parse.ast

import types.Type
import run.RunningInstance
import scala.util.parsing.combinator._
import java.math.BigInteger
import types._
import org.scalatest._
import scala.util.parsing.input.CharSequenceReader

class XprInt extends JavaTokenParsers {
  // Regex for valid identifiers.
  def id: Parser[String] = """[$[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*:]?]?[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*|!]""".r // EEK
  // note: ! is allowed, just not at the beginning (otherwise it has to be the only character)
  sealed trait Expression {
    def eval(ci: RunningInstance): Type
  }
  case class Literal(t: Type) extends Expression {
    def eval(ci: RunningInstance): Type = t
  }
  case class Variable(name: String) extends Expression {
    def eval(ci: RunningInstance): Type = ci.getVar(name)
  }
  def void: Parser[Expression] = "Void" ^^^ {new Literal(new TVoid())}
  def variable: Parser[Expression] = id ^^ {s => Variable(s)}
  def mountain: Parser[Expression] = wholeNumber ^^ {s => new Literal(new TMountain(new BigInteger(s)))}
  def hill: Parser[Expression] = """↼[-]?\d+""".r ^^ {s => new Literal(new THill(s.substring(1).toLong))}
  def string: Parser[Expression] = stringLiteral ^^ {s => new Literal(new TString(s))}
  def fish: Parser[Expression] = floatingPointNumber ^^ {s => new Literal(new TFish(s.toFloat))}
  def literal: Parser[Expression] = void | mountain | hill | string | fish
  def expression: Parser[Expression] = literal | variable
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
      parsing("29") should equal (Literal(new TMountain(new BigInteger("29"))))
      parsing("533") should equal (Literal(new TMountain(new BigInteger("533"))))
      parsing("533644755866977533644755866977") should equal (Literal(new TMountain(new BigInteger("533644755866977533644755866977"))))
      parsing("-2") should equal (Literal(new TMountain(new BigInteger("-2"))))
    }
    "XprInt" should "parse hills" in {
      implicit val parserToTest = hill
      parsing("↼90") should equal (Literal(new THill(90L)))
      parsing("↼95134") should equal (Literal(new THill(95134L)))
      parsing("↼-1") should equal (Literal(new THill(-1L)))
    }
    "XprInt" should "parse strings" in {
      implicit val parserToTest = string
      parsing("\"Nadenva\"") should equal (Literal(new TString("Nadenva")))
      parsing("\"\\\"Nadenva\"") should equal (Literal(new TString("\"Nadenva")))
    }
  }
}


