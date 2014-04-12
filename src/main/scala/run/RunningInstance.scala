package run

import java.io.FileInputStream
import scala.collection.mutable._
import types._
import util._
import rwvar._
import cmdreader.Global
import parse.ast._
import java.io.File

// If fname starts with "code:", then it is an instance of code.
/**
 * A class to define a running instance of a program.
 * @author bluebear94
 * @param fn the file name
 * @param c the next running instance on the stack
 * @param args the arguments
 */
class RunningInstance(fn: String, c: RunningInstance, args: Array[Type]) {
  // initial stuffs
  /**
   * The bytecode of the program.
   */
  var bytecode: Array[Byte] = Array[Byte]()
  /*if (!fname.startsWith("code:")) {
    val f: FileInputStream = new FileInputStream(fname)
    val bc = ArrayBuffer[Byte]()
    var b: Int = 0
    while (b != 1) {
      b = f.read()
      if (b != 1) {
        bc.append(b.asInstanceOf[Byte])
      }
    }
    bytecode = bc.toArray
  }*/
  val needle: Int = 0
  val calling = c
  val fname = fn
  /**
   * A list of variables and their values.
   */
  var environment = new HashMap[String, Type]()
  /**
   * The computation stack, with the first element on top.
   */
  var stack = List[Type]()
  /**
   * The symbolic stack (holds lvalues), with the first element on top.
   */
  var symstack = List[LValue]()
  /**
   * The last-answer variable.
   */
  var ans: Type = new TVoid
  /**
   * The last-answer variable, but doesn't update to a void value.
   */
  var answer: Type = new TVoid
  /**
   * Gets the value of the variable.
   * @param name the variable name
   * @return the value stored in the variable
   */
  def getVar(name: String): Type = {
    if (name.startsWith("$")) {
      // TODO A global variable or a command.
      if (name.indexOf(":") != -1) {
        new TCmdFunc(name.substring(1))
      } else {
        val p = PathNameConverter.aToOs(name.substring(1), false)
        VariableReader.readFile(p match {
          case (pn, true) => Global.current + "/" + pn
          case (pn, false) => Global.root + "/" + pn
        })
      }
    } else {
      if (environment.isDefinedAt(name)) {
        environment(name)
      } else {
        if (calling != null) {
          calling.getVar(name)
        } else {
          new TVoid
        }
      }
    }
  }
  /**
   * Sets the value of the variable.
   * @param name the variable name
   * @param t the value to store
   */
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
  /**
   * Deletes a variable.
   * @param name the variable name
   */
  def delVar(name: String): Unit = {
    if (name.startsWith("$")) {
      // TODO A global variable or a command.
      if (name.indexOf(":") == -1) {
        val p = PathNameConverter.aToOs(name.substring(1), false)
        new File(p match {
          case (pn, true) => Global.current + "/" + pn
          case (pn, false) => Global.root + "/" + pn
        }).delete
      }
    } else {
      environment.remove(name)
    }
  }
  /**
   * Gets the i<sup>th</sup> argument, one-indexed.
   * @param i the argument index to recall
   * @return the i<sup>th</sup> argument, or if <code>i == 0</code>, the number of arguments
   */
  def argn(i: Int): Type = {
    i match {
      case 0 => new THill(args.length)
      case _ => {
        if (i > 0 && i <= args.length) args(i - 1)
        else new TError(6)
      }
    }
  }
  /**
   * Stores a value into an argument.
   * @param i the argument index to recall
   * @param t the value to store
   */
  def setargn(i: Int, t: Type): Type = {
    if (i > 0) {
      args(i - 1) = t
      new TVoid
    } else new TError(6)
  }
  /**
   * Same as setVar, but looks for the variable in the calling program.
   */
  def setVarP(name: String, t: Type): Unit = {
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
      if (environment.isDefinedAt(name)) environment(name) = t
      else if (calling != null) calling.setVarP(name, t)
      else new TError(6)
    } 
  }
  /**
   * Gets the value of the variable.
   * @param name the variable name
   * @param r how deep into the call stack to recall (0 is this entry)
   * @return the value stored in the variable
   */
  def getVar(name: String, r: Int): Type = {
    if (r == 0) getVar(name)
    else calling.getVar(name, r - 1)
  }
  /**
   * Sets the value of the variable.
   * @param name the variable name
   * @param t the value to store
   * @param r how deep into the call stack to recall (0 is this entry)
   */
  def setVar(name: String, t: Type, r: Int): Unit = {
    if (r == 0) setVar(name, t)
    else calling.setVar(name, t, r - 1)
  }
  /**
   * Gets the i<sup>th</sup> argument, one-indexed.
   * @param i the argument index to recall
   * @param r how deep into the call stack to recall (0 is this entry)
   * @return the i<sup>th</sup> argument, or if <code>i == 0</code>, the number of arguments
   */
  def argn(i: Int, r: Int): Type = {
    if (r == 0) argn(i)
    else calling.argn(i, r - 1)
  }
  /**
   * Stores a value into an argument.
   * @param i the argument index to recall
   * @param t the value to store
   * @param r how deep into the call stack to recall (0 is this entry)
   */
  def setargn(i: Int, t: Type, r: Int): Type = {
    if (r == 0) setargn(i, t)
    else calling.setargn(i, t, r - 1)
  }
  /**
   * Same as setVar, but looks for the variable in the calling program.
   */
  def setVarP(name: String, t: Type, r: Int): Unit = {
    if (r == 0) setVarP(name, t)
    else calling.setVarP(name, t, r - 1)
  }
  /**
   * Reads an integer value at an index on the bytecode.
   * @param n the index
   * @return the integer value represented by the four bytes at that location
   * @throws ArrayIndexOutOfBoundsException when <code>n &gt; bytecode.length - 4</code>
   */
  def readInt(n: Int) = {
    (tus(bytecode(n)) << 24) + (tus(bytecode(n + 1)) << 16) + (tus(bytecode(n + 2)) << 8) + tus(bytecode(n + 3))
  }
  /**
   * Reads a UTF-8 string at an index on the bytecode.
   * @param n the index
   * @return the string value represented by the bytes at that location
   * @throws ArrayIndexOutOfBoundsException when needed
   */
  def readString(n: Int) = {
    val length = readInt(n)
    val strSt = n + 4
    (new String(bytecode.slice(strSt, strSt + length), "UTF-8"), strSt + length)
  }
  def tus(n: Byte) = if (n >= 0) n else 0x100 + n
  /**
   * Prints a stacktrace; i. e. a line for this program and the stacktrace for the calling program, if any.
   */
  def printStackTrace = {
    println("Stacktrace: ")
    var curNode = this
    while (curNode != null) {
      println((curNode.needle - 2).toHexString + "@" + curNode.fname)
      curNode = curNode.calling
    }
  }
  /**
   * Runs the bytecode in this instance.
   * @throws RuntimeException when encountering an invalid command, or encountering an error value on the stack.
   */
  def run = { // runs the bytecode
    var needle = 0
    var isDone = false
    while (!isDone) {
      val cmd = (tus(bytecode(needle)) << 8) + tus(bytecode(needle + 1))
      needle += 2
      cmd match {
        case 0xE000 => {
          val function = stack.head
          stack = stack.tail
          val argcount = readInt(needle)
          val (args, ns) = stack.splitAt(argcount)
          stack = ns
          val realArgs = args.reverse.toArray
          val toPush = function match {
            case f: TFunction => f(if (Global.vigilant) realArgs.map(_.>/<) else realArgs)
            case _ => new TError(1)
          }
          stack = toPush :: stack
          needle += 4
        }
        case 0xE004 => {
          val (name, newIndex) = readString(needle)
          stack = getVar(name) :: stack
          needle = newIndex
        }
        case 0xE005 => {
          val (name, newIndex) = readString(needle)
          symstack = Variable(name) :: symstack
          needle = newIndex
        }
        case 0xE932 => {
          needle = if (stack.head.toBoolean) needle + 4 else readInt(needle)
          stack = stack.tail
        }
        case 0xE933 => {
          needle = if (stack.head.toBoolean) readInt(needle) else needle + 4
          stack = stack.tail
        }
        case 0xE934 => {
          needle = readInt(needle)
        }
        case 0xE935 => isDone = true
        case 0xE938 => {
          stack = Hashtag(Literal(stack.head)).eval(this) :: stack.tail
        }
        case 0xE948 => {
          symstack = Hashtag(Literal(stack.head)) :: symstack.tail
          stack = stack.tail
        }
        case 0xE939 => {
          val element = stack.head
          val list = stack(1)
          stack = Indexing.index(list, element) :: stack.tail.tail
        }
        case 0xE949 => {
          val element = stack.head
          val list = symstack.head
          stack = stack.tail
          symstack = LIndex(list, Literal(element)) :: symstack.tail
        }
        case 0xE940 => {
          val argcount = readInt(needle)
          val (args, ns) = stack.splitAt(argcount)
          stack = ns
          val toPush = new LArray(args.toBuffer.to[ArrayBuffer])
          stack = toPush :: stack
          needle += 4
        }
        case 0xE945 => {
          val argcount = readInt(needle)
          val (args, ns) = stack.splitAt(argcount)
          stack = ns
          val toPush = new LLinked(args.toBuffer.to[ListBuffer])
          stack = toPush :: stack
          needle += 4
        }
        case 0xE950 => {
          val toStore = stack.head
          val sym = symstack.head
          sym.assign(this, toStore)
          symstack = symstack.tail
        }
        case 0xEA50 => {
          val toStore = stack.head
          val sym = symstack.head
          sym.assignS(this, toStore)
          symstack = symstack.tail
        }
        case 0xE951 => {
          stack = stack.head :: stack
        }
        case 0xE952 => {
          stack = stack.tail
        }
        case 0xE953 => {
          ans = stack match {
            case Nil => new TVoid
            case f :: r => f match {
              case e: TError => ans
              case _ => f
            }
          }
          ans match {
            case v: TVoid => ()
            case _ => answer = ans
          }
          stack = stack match {
            case Nil => Nil
            case f :: r => r
          }
        }
        case 0xE954 => stack = ans :: stack
        case 0xE955 => stack = answer :: stack
        case 0xE956 => {
          symstack.head.nuke(this)
          symstack = symstack.tail
          stack = new TVoid :: stack
        }
        case _ => {
          if ((cmd >> 8) == 0xE1) {
            val valtype = cmd & 0xFF
            val size = readInt(needle)
            needle += 4
            stack = VariableReader.readData(bytecode.slice(needle, needle + size), valtype, "[ANON]") :: stack
            needle += size
          } else {
            printStackTrace
            throw new RuntimeException("Invalid command: " + cmd + "@" + (needle - 2).toHexString)
          }
        }

      }
      if (needle >= bytecode.length - 1) isDone = true
      if (!stack.isEmpty && stack.head.isInstanceOf[TError]) {
        val e = stack.head
        stack = stack.tail
        printStackTrace
        throw new RuntimeException("Runtime " + e + ": " + (needle - 2).toHexString + "@" + fname + ": " +
          cmd.toHexString)
      }
    }
  }
}