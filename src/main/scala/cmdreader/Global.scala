package cmdreader

import scala.collection.mutable.HashMap
import java.io._
import java.lang._
import run.RunningInstance
import java.math.BigInteger

object Global {
  var liblist: HashMap[String, CmdList] = new HashMap[String, CmdList]()
  var top = new RunningInstance("code: testing", null, Array())
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
  def getCmdno(name: String): Command = {
    val i = name.indexOf(":")
    val lib = if (i == 0) "std" else name.substring(0, i)
    val op = name.substring(i + 1)
    Global.liblist(lib).commandList(op)
  }
  var root: File = new File("amw/")
  var current: File = root
  var currentAlias = "root"
  val TWO = new BigInteger("2")
  val vM = 0
  val vm = 5
  val vr = 3
  val version = vM + "." + vm + "." + vr
}