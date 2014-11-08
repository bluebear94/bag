package Main

// this is the real shit

import logger.Logger
import scopt._
import cmdreader.Global
import java.io.File

object Main {
  def main(args: Array[String]) {
    val parser = new OptionParser[Config]("bag") {
      head("Bag", Global.version)
      opt[Int]('v', "verbosity") action { (x, c) =>
        c.copy(verbosity = x) } text("verbosity setting: -3 will print out everything to STDOUT; 3 will only print critical errors. In any case, a file will be saved.)")
      opt[Unit]("nogui") action { (_, c) =>
        c.copy(gui = false) } text("do not display the GUI")
      opt[String]('c', "compile") action { (x, c) =>
        c.copy(file = Some(x), compile = true) } text("compile a .bag file")
      opt[Unit]('a', "a") action { (_, c) =>
        c.copy(avc = true) } text("enable automatic value copying")
      help("help") text("prints this usage text")
    }
    parser.parse(args, Config()) map { config =>
      try {
        Logger.open()
        Logger.verbosity = config.verbosity
        Logger.println("This is Bag version " + Global.version)
        Global.vigilant = config.avc
        if (config.gui) gui.Main.main(Array[String]())
      } catch {
        case e: Throwable => {
          try {Logger.println(e.getMessage, 3)}
          catch {case e: RuntimeException => ()}
          finally {
            Logger.printStackTrace(e)
            System.exit(2)
          }
        }
      } finally {
      }
    } getOrElse {}
    //Logger.close()
  }
}

case class Config(verbosity: Int = 3, gui: Boolean = true, file: Option[String] = None, compile: Boolean = false, avc: Boolean = false)
