package cmdreader

import scala.collection.mutable.HashMap
import java.io._
import java.lang._
import run.RunningInstance
import java.math.BigInteger
import scala.util.Random
import logger.Logger

/**
 * Global properties.
 */

object Global {
  /**
   * The list of libraries loaded. By default, std is.
   */
  var liblist: HashMap[String, CmdList] = new HashMap[String, CmdList]()
  /**
   * The current running instance.
   */
  var top = new RunningInstance("code: testing", null, Array())
  /**
   * Loads a library with a given name.
   * @param lname the library name, which must be a valid identifier
   */
  def loadLib(lname: String) = {
    //Logger.println(s"Loading library $lname", 2)
    liblist(lname) = new CmdList(lname)
    val c = Class.forName("cmdreader." + lname + ".Loader")
    c.getMethod("load").invoke(c.newInstance)
  }
  /**
   * Gets the operator with the given name.
   */
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
  /**
   * Gets the command with the given name.
   * Results are undefined if the name does not start with a dollar sign.
   */
  def getCmdno(name: String): Command = {
    val i = name.indexOf(":")
    val lib = if (i == 0) "std" else name.substring(0, i)
    val op = name.substring(i + 1)
    Global.liblist(lib).commandList(op)
  }
  /**
   * The root directory for persistent variables.
   */
  var root: File = new File("amw/")
  /**
   * The current directory for persistent variables.
   */
  var current: File = root
  /**
   * The current directory in Amethyst path format.
   */
  var currentAlias = "root"
  @deprecated("Due to use of scala.math.BigInt instead.", "0.5.13")
  val TWO = new BigInteger("2")
  /*
   * The major version number.
   */
  val vM = 0
  /*
   * The minor version number.
   */
  val vm = 7
  /*
   * The revision number.
   */
  val vr = 253 // time for a new test image!
  val vrr = "-alphaDev"
  val version = "v" + vM + "." + vm + "." + vr + vrr
  val r: Random = new Random
  var vigilant = false
}
