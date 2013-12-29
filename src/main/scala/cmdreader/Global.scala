package cmdreader

import scala.collection.mutable.HashMap
import java.io._
import java.lang._

object Global {
  var liblist: HashMap[String, CmdList] = new HashMap[String, CmdList]()
  def loadLib(lname: String) = {
    liblist(lname) = new CmdList(lname)
    val c = Class.forName("cmdreader." + lname + ".Loader")
    c.getMethod("load").invoke(c.newInstance)
  }
  def getCmd(name: String): CommandOperator = {
    if (name.startsWith("$")) {
      val i = name.indexOf(":")
      val lib = name.substring(0, i)
      val op = name.substring(i + 1)
      Global.liblist(lib).ccol.opList(op)
    } else {
      Global.liblist("std").ccol.opList(name)
    }
  }
  var root: File = new File("amw/")
  var current: File = root
}