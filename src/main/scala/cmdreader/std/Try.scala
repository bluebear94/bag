package cmdreader.std

import cmdreader._
import types._
import run._

class Try extends CommandOperator {
  def getName(): String = "try"
  def getOpAlias() = "#>"
  def isValidArg0(n: Int) = n == 2
  def apply(args: Array[Type]): Type = {
    def err(x: Type) = new TError(1, s"$x cannot be treated as a function")
    args(0) match {
      case code: FuncLike => args(1) match {
        case exn: FuncLike => {
          try {
            code(Array[Type]())
          } catch {
            case e: BagException => exn(Array[Type](TMountain(e.e.errno)))
          }
        }
        case x => err(x)
      }
      case x => err(x)
    }
  }
  def getPrecedence() = PStandard.TRY
  def isReversed() = false
  def hasAssignmentEquiv() = false
  def getDoubleBase() = None
}
