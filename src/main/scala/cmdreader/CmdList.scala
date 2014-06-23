package cmdreader
import java.lang._
import scala.collection.mutable.HashMap
import eloro._
import scala.io.Source
import gui.Main

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
  def loadCmd(cname: String, lines: List[String]) = {
    val c = Class.forName("cmdreader." + libname + "." + cname)
    val inst = c.newInstance().asInstanceOf[Command]
    val ilname = inst.getName
    commandList(ilname.asInstanceOf[String]) = inst
    inst match {
      case co: CommandOperator => ccol.loadOp(co)
      case _ => ()
    }
    println(s"Loaded command $ilname")
    if (!(lines contains ("#" + ilname))) {
      println(s"*** $ilname is undocumented")
      Main.println(s"*** $ilname is undocumented")
    }
  }
}
