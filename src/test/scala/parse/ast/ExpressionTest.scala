package parse.ast

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.input.CharSequenceReader
import types._
import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import cmdreader.Global

class ExpressionParsersTest extends FlatSpec with Matchers {
  val parser = new XprInt()
  Global.loadLib("std")
  parser.loadOps
  import parser._

  private def parsing[T](s: String)(implicit p: Parser[T]): T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException(
        "Could not parse '" + s + "': " + msg)
    }
  }
  private def assertFail[T](input: String)(implicit p: Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }
  "XprInt" should "parse void literals" in {
    implicit val parserToTest = void
    parsing("Void") should equal(Literal(new TVoid))
    assertFail("void")
  }
  "XprInt" should "parse mountains" in {
    implicit val parserToTest = mountain
    parsing("29") should equal(Literal(TMountain(new BigInteger("29"))))
    parsing("533") should equal(Literal(TMountain(new BigInteger("533"))))
    parsing("533644755866977533644755866977") should equal(Literal(TMountain(new BigInteger("533644755866977533644755866977"))))
    parsing("-2") should equal(Literal(TMountain(new BigInteger("-2"))))
  }
  "XprInt" should "parse hills" in {
    implicit val parserToTest = hill
    parsing("↼90") should equal(Literal(THill(90L)))
    parsing("↼95134") should equal(Literal(THill(95134L)))
    parsing("↼-1") should equal(Literal(THill(-1L)))
  }
  "XprInt" should "parse strings" in {
    implicit val parserToTest = string
    parsing("\"Nadenva\"") should equal(Literal(new TString("Nadenva")))
    parsing("\"\\\"Nadenva\"") should equal(Literal(new TString("\"Nadenva")))
  }
  "XprInt" should "parse fish" in {
    implicit val parserToTest = fish
    parsing("3.0") should equal(Literal(TFish(3.0)))
    parsing("3.") should equal(Literal(TFish(3.0)))
    parsing("15.6") should equal(Literal(TFish(15.6)))
    parsing("-1.98") should equal(Literal(TFish(-1.98)))
    parsing("0.9") should equal(Literal(TFish(0.9)))
    parsing(".5") should equal(Literal(TFish(.5)))
  }
  /*"XprInt" should "parse arrays and linked lists" in {
    implicit val parserToTest = array | linked
    parsing("{3, 4, 5}") should equal(AList(true, Array(
      Literal(TMountain(new BigInteger("3"))),
      Literal(TMountain(new BigInteger("4"))),
      Literal(TMountain(new BigInteger("5"))))))
    parsing("[3, 4, 5]") should equal(AList(false, Array(
      Literal(TMountain(new BigInteger("3"))),
      Literal(TMountain(new BigInteger("4"))),
      Literal(TMountain(new BigInteger("5"))))))
  }*/
  "XprInt" should "use operators" in {
    implicit val parserToTest = operator(ops.firstKey) // TODO: change the type of parser
    parsing("4 + 9") should equal(Operator("+", Literal(TMountain(new BigInteger("4"))), Literal(TMountain(new BigInteger("9")))))
    assertFail("4 +")
    parsing("7 - 3") should equal(Operator("-", Literal(TMountain(new BigInteger("7"))), Literal(TMountain(new BigInteger("3")))))
    //parsing("5 * 8") should equal(Operator("*", Literal(TMountain(new BigInteger("5"))), Literal(TMountain(new BigInteger("8")))))
    parsing("3 + 4 - 5") should equal(Operator("-", Operator("+", Literal(TMountain(new BigInteger("3"))),
        Literal(TMountain(new BigInteger("4")))), Literal(TMountain(new BigInteger("5")))))
  }
  "XprInt" should "handle variables" in {
    implicit val parserToTest = expression
    parsing("a = 3") should equal(Assign(Variable("a"), Literal(TMountain(new BigInteger("3")))))
    parsing("a += 1") should equal(AssignOp(Variable("a"), Literal(TMountain(new BigInteger("1"))), "+"))
  }
  "XprInt" should "handle more variables" in {
    implicit val parserToTest = variable
    parsing("$a") should equal(Variable("$a"))
    parsing("$std:add") should equal(Variable("$std:add"))
    parsing("$:add") should equal(Variable("$:add"))
  }
}