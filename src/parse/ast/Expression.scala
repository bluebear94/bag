package parse.ast

import types.Type
import run.RunningInstance
import scala.util.parsing.combinator._



object XprInt extends RegexParsers {
  def ident: Parser[String] = """[$[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*:]?]?[[^!@#$%^&*()_-=+~{}[]\|:;'",.<>/?][^@#$%^&*()_-=+~{}[]\|:;'",.<>/?]*|!]""".r // EEK
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
}