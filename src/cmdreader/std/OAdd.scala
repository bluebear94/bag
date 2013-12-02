package cmdreader.std

import cmdreader.CommandOperator
import types._
import util._

class OAdd extends CommandOperator {
  override def getName(): String = "add"
  override def getOpAlias() = "+"
  override def apply(args: Array[Type]): Type = {
    args.fold(
    if (args(0).getType == 3) new TString("")
    else new THill(0L)
    )(MathUtil.add(_, _))
  }
}