package eloro

import types._
import cmdreader._
import scala.util.parsing.combinator._
import scala.collection.immutable.HashMap
import scala.util.parsing.input._
import java.io._
import java.util.Scanner
import scala.io.Source
import scala.xml._
/**
 * Documentation generator. Called through the $:help command.
 * @author bluebear94
 */
object DocGen {
  def getProperties(cmd: Command): String = {
    val arglist = List.range(0, 20).filter(cmd.isValidArg0(_))
    val argstr = "\nValid argcounts: " + arglist.mkString(", ") + (if (arglist.filter(_ > 10).isEmpty) "" else ", ...")
    val opProperties = cmd match {
      case op: CommandOperator => {
        val alias = op.getOpAlias
        val hasae = op.hasAssignmentEquiv
        val db = op.getDoubleBase
        val prec = op.getPrecedence
        val un = op.isUnary
        val isrtl = op.isReversed
        val dm = db match {
          case Some(t) => {
            "\n" + alias + alias + "a is equivalent to a " + alias + "= " + t
          }
          case None => ""
        }
        "\nOperator equivalent: " + alias + "\nPrecedence:  " + prec + "\nUnary: " + (if (un) "Yes" else "No") + "\n" +
          (if (isrtl) "Right-to-left" else "Left-to-right") + (if (hasae) "\nAssignment Equiv.: " + alias + "=") + dm
      }
      case _ => ""
    }
    cmd.getName + argstr + opProperties
  }
  def getPropertiesX(cmd: Command): NodeSeq = {
    val arglist = List.range(0, 20).filter(cmd.isValidArg0(_))
    val argstr = <p>Valid argcounts: {arglist.mkString(", ") + (if (arglist.filter(_ > 10).isEmpty) "" else ", ...")}</p>
    val opProperties = cmd match {
      case op: CommandOperator => {
        val alias = op.getOpAlias
        val hasae = op.hasAssignmentEquiv
        val db = op.getDoubleBase
        val prec = op.getPrecedence
        val un = op.isUnary
        val isrtl = op.isReversed
        val dm = db match {
          case Some(t) => {
            <p> {alias + alias + "a is equivalent to "} <code>{"a " + alias + "= " + t}</code> </p>
          }
          case None => NodeSeq.Empty
        }
        <p> Operator equivalent: <code>{alias}</code> </p><p> Precedence: {prec} </p><p> Unary: {if (un) "Yes" else "No"} </p><p>
          {if (isrtl) "Right-to-left" else "Left-to-right"} </p><p> {if (hasae) "\nAssignment Equiv.: "} <code>{if (hasae) alias + "="}</code> </p><p> {dm} </p>
      }
      case _ => NodeSeq.Empty
    }
    argstr ++ opProperties
  }
  val cmdl: HashMap[String, (List[String]) => String] = HashMap(
    "desc" -> (_.head.replaceAll("<.*?>", "").replaceAll("&amp;", "&").replaceAll("&gt;", ">").replaceAll("&lt;", "<")),
    "vector" -> (_ => "If applied to one or more lists, this function will apply element-by-element."),
    "throws" -> ((a: List[String]) => "Throws error #" + a.mkString(", ")),
    "contract" -> ("Contract: " + _.mkString(" ")))
  val cmdx: HashMap[String, (List[String]) => NodeSeq] = HashMap(
    "desc" -> ((l: List[String]) => (XML.loadString("<p>" + l.head + "</p>"))),
    "vector" -> (_ => <p><i>If applied to one or more lists, this function will apply element-by-element.</i></p>),
    "throws" -> ((a: List[String]) => <p>{"Throws error #" + a.mkString(", ")}</p>),
    "contract" -> ((l: List[String]) => (<p>Contract: <code>{l.mkString(" ")}</code></p>)))
  def out(c: String, a: List[String]) = {
    if (cmdl.isDefinedAt(c)) cmdl(c)(a)
    else throw new NoSuchDocCommandException("Command not found: " + c)
  }
  def outx(c: String, a: List[String]) = {
    if (cmdx.isDefinedAt(c)) cmdx(c)(a)
    else throw new NoSuchDocCommandException("Command not found: " + c)
  }
  def parseCmd(ln: String) = {
    val p = new EP
    import p._
    val pp = phrase(line)
    val ret = pp(new CharSequenceReader(ln))
    ret match {
      case Success(res, t) => out(res._1, res._2)
      case NoSuccess(msg, t) => {
        println(ret)
        throw new RuntimeException("Could not parse: " + msg + " in line: " + ln)
      }
    }
  }
  def parseCmdX(ln: String) = {
    val p = new EP
    import p._
    val pp = phrase(line)
    val ret = pp(new CharSequenceReader(ln))
    ret match {
      case Success(res, t) => outx(res._1, res._2)
      case NoSuccess(msg, t) => {
        println(ret)
        throw new RuntimeException("Could not parse: " + msg + " in line: " + ln)
      }
    }
  }
  def getHelp(name: String) = {
    if (name.charAt(0) != '$' || name.indexOf(":") == -1) "Invalid command name"
    else {
      val bp = getProperties(Global.getCmdno(name.substring(1)))
      val l = name.substring(1, name.indexOf(":"))
      val n = name.substring(name.indexOf(":") + 1)
      val lib = if (l.isEmpty) "std" else l
      val s = Source.fromFile(s"docs/$lib.txt")
      var hasFoundHeader = 0
      var details = ""
      def readAndAppend(l: String) = {
        if (!l.startsWith("#")) details += "\n" + parseCmd(l)
      }
      for (l <- s.getLines if hasFoundHeader != 2) {
        if (hasFoundHeader == 1) readAndAppend(l)
        if (l == "#" + n) hasFoundHeader = 1
        else if ((l startsWith "#") && hasFoundHeader == 1) hasFoundHeader = 2
      }
      bp + details
    }
  }
  def getHelpX(n: String, l: String) = {
    val bp = getPropertiesX(Global.getCmdno(s"$l:$n"))
    val lib = if (l.isEmpty) "std" else l
    val s = Source.fromFile(s"docs/$lib.txt")
    var hasFoundHeader = 0
    var details = NodeSeq.Empty
    def readAndAppend(l: String) = {
      if (!l.startsWith("#")) details ++= parseCmdX(l)
    }
    for (l <- s.getLines if hasFoundHeader != 2) {
      if (hasFoundHeader == 1) readAndAppend(l)
      if (l == "#" + n) hasFoundHeader = 1
      else if ((l startsWith "#") && hasFoundHeader == 1) hasFoundHeader = 2
    }
    bp ++ details
  }
  def getLibInfo(name: String): String = {
    try {
      val s = Source.fromFile(s"docs/$name.txt")
      var hasFoundHeader = 0
      var r = ""
      for (l <- s.getLines) {
        if (l == "###") hasFoundHeader = 1
        else if (hasFoundHeader == 1 && (l startsWith "#")) return r
        else if (hasFoundHeader == 1) r += l + "\n"
      }
      r
    } catch {
      case e: FileNotFoundException => "This library does not exist."
    }
  }
  def getLibInfoX(name: String): NodeSeq = {
    try {
      val s = Source.fromFile(s"docs/$name.txt")
      var hasFoundHeader = 0
      var r = NodeSeq.Empty
      for (l <- s.getLines) {
        if (l == "###") hasFoundHeader = 1
        else if (hasFoundHeader == 1 && (l startsWith "#")) return r
        else if (hasFoundHeader == 1) r ++= <p> {l} </p>
      }
      r
    } catch {
      case e: FileNotFoundException => XML.loadString("This library does not exist.")
    }
  }
  def genLibX(name: String) = {
    val ll = Global.liblist(name)
    val cl = ll.commandList
    val cmds = cl.keys.toList.sorted
    <html>
      <head>
        <title>{name} - Bag Documentation</title>
      </head>
      <body>
        <h1>{name}</h1>
        <h2>{DocGen.getLibInfoX(name)}</h2>
        {
          cmds.flatMap { (c) =>
            <a href={s"#$c"}>
              {c}
            </a>
          }
        }
        {
          cmds.flatMap { (c) =>
            <a id={c}>
              <h2>{c}</h2>
              {getHelpX(c, name)}
            </a>
          }
        }
        <p align="center">
          Generated using an automatic tool. To produce an HTML page for <i>your</i> library, write up a documentation file and generate the page using <code>$:help(5, &quot;<i>library name</i>&quot;)</code>.
        </p>
      </body>
    </html>
  }
  def genLibXS(name: String) = {
    /*val f = new File(s"docs/html/$name.html")
    f.getParentFile.mkdirs()
    f.createNewFile()
    val ps = new PrintStream(f)
    ps.print(genLibX(name))
    ps.close()*/
    XML.save(s"docs/html/$name.html", genLibX(name), "UTF-8")
  }
}
class NoSuchDocCommandException(msg: String) extends RuntimeException
/**
 * Parser class for doc commands.
 */
class EP extends RegexParsers with PackratParsers {
  override def skipWhitespace = false
  lazy val quoted: PackratParser[String] = "\"" ~> "[^\"]*".r <~ "\""
  lazy val unquoted: PackratParser[String] = regex("[^\"\\s]*".r)
  lazy val args: PackratParser[List[String]] = repsep(quoted | unquoted, " ")
  lazy val lineA: PackratParser[(String, List[String])] = unquoted ~ " " ~ args ^^ {
    case uq ~ _ ~ a => (uq, a)
  }
  lazy val lineB: PackratParser[(String, List[String])] = unquoted ^^ {s: String => (s, Nil)}
  lazy val line = lineA | lineB
}
