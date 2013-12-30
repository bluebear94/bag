package types

import parse.ast._
import run.RunningInstance
import cmdreader.Global

class TASTFunc(lines: List[Expression], ci: RunningInstance) extends TFunction {
  def apply(args: Array[Type]): Type = {
    val newci = new RunningInstance("code: fcall", Global.top, args)
    Global.top = newci
    val res = lines.map(_.eval(Global.top)).last
    Global.top = Global.top.calling
    res
  }
  def >/< = new TASTFunc(lines.toList, ci)
}