package logger

import scala.util.Random
import java.io.{File, PrintStream}

// royo

object Logger {
  private var ps: Option[PrintStream] = None
  def printStream: Option[PrintStream] = ps
  var verbosity: Int = 3
  def open() {
    val r = new Random(System.currentTimeMillis)
    var fname = "log/"
    for (i <- 0 until 4) fname += nextWord(r)
    fname += ".txt"
    val file = new File(fname)
    try {
      (new File("log")).mkdirs()
      file.createNewFile()
    } catch {
      case e: Exception => {
        System.out.println("Could not create log file: " + e.getMessage)
        System.exit(1)
      }
    }
    ps = Some(new PrintStream(file))
    println("This is bb94's ROYO logger")
    println(s"Generated ${new java.util.Date()}")
  }
  private val words = List(
    "Aki",
    "Asa",
    "Ki",
    "Inu",
    "Neko",
    "Mori",
    "Kawa",
    "Yama",
    "Tsuki",
    "Hi",
    "Mizu",
    "Tsuchi",
    "Ishi",
    "Kusa",
    "Tane",
    "Aka"
  )
  private def nextWord(r: Random) = words((r.nextLong.abs % words.length).toInt)
  def print(msg: String, priority: Int = 0) {
    ps match {
      case None => throw new RuntimeException("File is not opened")
      case Some(s) => s.print(msg)
    }
    if (priority >= verbosity) print(msg)
  }
  def println(msg: String, priority: Int = 0) = print(msg + "\n", priority)
  def close() = {
    ps match {
      case None => ()
      case Some(s) => s.close()
    }
    ps = None
  }
  def printStackTrace(e: Throwable) = {
    printStream match {
      case None => e.printStackTrace()
      case Some(s) => e.printStackTrace(s)
    }
  }
}
