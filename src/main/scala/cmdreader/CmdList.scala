package cmdreader
import java.lang._
import scala.collection.mutable.HashMap

/**
 * A list of commands in a library.
 * @author bluebear94
 */
class CmdList(libname: String) {
  // A list of commands in one library.
  /**
   * Returns the library name.
   */
  def getLib(): String = libname
  /**
   * A map with the names corresponding to their respective commands.
   */
  var commandList: HashMap[String, Command] = new HashMap[String, Command]()
  /**
   * The corresponding list of operators.
   */
  var ccol = new CmdOpList(libname)
  /**
   * Loads a command.
   */
  def loadCmd(cname: String) = {
    val c = Class.forName("cmdreader." + libname + "." + cname)
    val inst = c.newInstance()
    val ilname = c.getMethod("getName").invoke(inst)
    commandList(ilname.asInstanceOf[String]) = inst.asInstanceOf[Command]
    inst match {
      case co: CommandOperator => ccol.loadOp(co)
      case _ => ()
    }
    print(s"Loaded command $ilname\n")
  }
}