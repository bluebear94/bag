package types

import parse.ast._
import run.RunningInstance

class TASTFunc(lines: List[Expression], ci: RunningInstance) extends TFunction {
  def apply(args: Array[Type]): Type = {
    lines.map(_.eval(ci)).last
  }
}