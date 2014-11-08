package cmdreader

import scala.collection.mutable.HashMap
import logger.Logger

class CmdOpList(libname: String) {
  def getLib(): String = libname
  var opList: HashMap[String, CommandOperator] = new HashMap[String, CommandOperator]()
  def loadOp(c: CommandOperator) {
    val opn = c.getOpAlias
    opList(opn) = c
    Logger.println(s"Loaded operator $opn")
  }
}
