package run

import java.io.FileInputStream
import scala.collection.mutable._
import types._
import util._
import rwvar.VariableWriter
import cmdreader.Global

// If fname starts with "code:", then it is an instance of code.
class RunningInstance(fname: String, c: RunningInstance, args: Array[Type]) {
  // initial stuffs
  if (!fname.startsWith("code:")) {
    val f: FileInputStream = new FileInputStream(fname)
    val bytecode: Buffer[Byte] = ArrayBuffer[Byte]()
    var b: Int = 0
    while (b != 1) {
      b = f.read()
      if (b != 1) {
        bytecode.append(b.asInstanceOf[Byte])
      }
    }
  }
  val needle: Int = 0
  val calling = c
  var environment = new HashMap[String, Type]()
  var stack = List()
  def getVar(name: String): Type = {
    if (name.startsWith("$")) {
      // TODO A global variable or a command.
      if (name.indexOf(":") == -1) {
        new TCmdFunc(name)
      } else {
        new TVoid // TODO over here
      }
    } else {
      if (environment.isDefinedAt(name)) {
        environment(name)
      } else {
        if (calling != null) {
          calling.getVar(name).>/< // make sure to strip away any references
        } else {
          new TVoid
        }
      }
    }
  }
  def setVar(name: String, t: Type): Unit = {
    if (name.startsWith("$")) {
      // TODO A global variable or a command.
      if (name.indexOf(":") == -1) {
        val p = PathNameConverter.aToOs(name.substring(1), false)
        VariableWriter.writeValToVarfile(t, p match {
          case (pn, true) => Global.current + "/" + pn
          case (pn, false) => Global.root + "/" + pn
        })
      }
    } else {
      environment(name) = t
    }
  }
  def argn(i: Int): Type = {
    i match {
      case 0 => new THill(args.length)
      case _ => if (i > 0) args(i - 1)
      else new TError(6)
    }
  }
  def setargn(i: Int, t: Type): Type = {
    if (i > 0) {
      args(i - 1) = t
      new TVoid
    } else new TError(6)
  }
  def getVar(name: String, r: Int): Type = {
    if (r == 0) getVar(name)
    else calling.getVar(name, r - 1)
  }
  def setVar(name: String, t: Type, r: Int): Unit = {
    if (r == 0) setVar(name, t)
    else calling.setVar(name, t, r - 1)
  }
  def argn(i: Int, r: Int): Type = {
    if (r == 0) argn(i)
    else calling.argn(i, r - 1)
  }
  def setargn(i: Int, t: Type, r: Int): Type = {
    if (r == 0) setargn(i, t)
    else calling.setargn(i, t, r - 1)
  }
}