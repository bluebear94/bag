package run

import java.io.FileInputStream
import scala.collection.mutable._
import types._

// If fname starts with "code:", then it is an instance of code.
abstract class RunningInstance(fname: String, c: RunningInstance, args: Array[Type]) {
  // initial stuffs
  val f: FileInputStream = new FileInputStream(fname)
  val bytecode: Buffer[Byte] = ArrayBuffer[Byte]()
  var b: Int = 0
  while (b != 1) {
    b = f.read()
    if (b != 1) {
      bytecode.append(b.asInstanceOf[Byte])
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
      }
      else {
        new TVoid // TODO over here
      }
    }
    else {
      if (environment.isDefinedAt(name)) {
        environment(name)
      }
      else {
        if (calling != null) {
          calling.getVar(name)
        }
        else {
          new TVoid()
        }
      }
    }
  }
  def setVar(name: String, t: Type): Unit = {
    if (name.startsWith("$")) {
      // TODO A global variable or a command.
      if (name.indexOf(":") == -1) {
        // TODO over here
      }
    }
    else {
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
    }
    else new TError(6)
  }
}