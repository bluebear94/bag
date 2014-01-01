package run

import java.io.FileInputStream
import scala.collection.mutable._
import types._
import util._
import rwvar._
import cmdreader.Global
import parse.ast._

// If fname starts with "code:", then it is an instance of code.
class RunningInstance(fname: String, c: RunningInstance, args: Array[Type]) {
  // initial stuffs
  var bytecode: Array[Byte] = Array[Byte]()
  if (!fname.startsWith("code:")) {
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
  }
  val needle: Int = 0
  val calling = c
  var environment = new HashMap[String, Type]()
  var stack = List[Type]()
  var symstack = List[LValue]()
  var ans: Type = new TVoid
  var answer: Type = new TVoid
  def getVar(name: String): Type = {
    if (name.startsWith("$")) {
      // TODO A global variable or a command.
      if (name.indexOf(":") == 1) {
        new TCmdFunc(name.substring(1))
      } else {
        new TVoid // TODO over here
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
      case _ => {
        if (i > 0 && i <= args.length) args(i - 1)
        else new TError(6)
      }
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
  def readInt(n: Int) = {
    (tus(bytecode(n)) << 24) + (tus(bytecode(n + 1)) << 16) + (tus(bytecode(n + 2)) << 8) + tus(bytecode(n + 3))
  }
  def readString(n: Int) = {
    val length = readInt(n)
    val strSt = n + 4
    (new String(bytecode.slice(strSt, strSt + length), "UTF-8"), strSt + length)
  }
  def tus(n: Byte) = if (n >= 0) n else 0x100 + n
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
          val toPush = function match {
            case f: TFunction => f(args.reverse.toArray)
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
        case 0xE951 => {
          stack = stack.head :: stack
        }
        case 0xE952 => {
          stack = stack.tail
        }
        case 0xE953 => {
          ans = stack.head
          ans match {
            case v: TVoid => ()
            case _ => answer = ans
          }
          stack = stack.tail
        }
        case _ => {
          if ((cmd >> 8) == 0xE1) {
            val valtype = cmd & 0xFF
            val size = readInt(needle)
            needle += 4
            stack = VariableReader.readData(bytecode.slice(needle, needle + size), valtype) :: stack
            needle += size
          } else {
            throw new RuntimeException("Invalid command: " + cmd)
          }
        }
        
      }
      if (needle >= bytecode.length - 1) isDone = true
      if (!stack.isEmpty && stack.head.isInstanceOf[TError]) {
        throw new RuntimeException("Runtime " + stack.head)
      }
    }
  }
}