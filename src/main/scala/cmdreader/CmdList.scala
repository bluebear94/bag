package cmdreader
import java.lang._
import scala.collection.mutable.HashMap

class CmdList(libname: String) {
  // A list of commands in one library.
  def getLib(): String = libname
  var commandList: HashMap[String, Command] = new HashMap[String, Command]()
  val ccol = new CmdOpList(libname)
  def loadCmd(cname: String) = {
    val c = Class.forName("cmdreader." + libname + "." + cname)
    val inst = c.newInstance()
    val ilname = c.getMethod("apply").invoke(inst)
    commandList(ilname.asInstanceOf[String]) = inst.asInstanceOf[Command]
    if (c.isInstanceOf[CommandOperator]) ccol.loadOp(c.asInstanceOf[CommandOperator])
  }
}