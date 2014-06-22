package types

import parse.ast._
import run.RunningInstance
import cmdreader.Global
import scala.collection.mutable.HashMap

@deprecated("This isn't even supposed to be used anymore...", "0.7.1")
class TASTFunc(lines: List[Expression], ci: RunningInstance) extends TFunction {
  def applyWith(args: Array[Type], closure: HashMap[String, Type]) = {
    val newci = new RunningInstance("code: fcall", Global.top, args)
    Global.top = newci
    val res = lines.map(_.eval(Global.top)).last
    Global.top = Global.top.calling
    res
  }
  def >/< = new TASTFunc(lines.toList, ci)
}