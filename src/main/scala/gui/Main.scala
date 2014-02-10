package gui

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.awt.{ Graphics2D, Color, Dimension, Font }
import java.awt.image.BufferedImage
import cmdreader.Global
import parse.ast._
import scala.util.parsing.input.CharSequenceReader
import types.TVoid
import scala.collection.mutable.ArrayBuffer


// I'm a complete noob to this...

/**
 * The graphic user interface for the language.
 * @author bluebear94
 */
object Main extends SimpleSwingApplication {
  /**
   * Standard monospace font.
   */
  val mono = new Font("DejaVu Sans Mono", 0, 10)
  /**
   * A parser.
   */
  val p = new XprInt
  /**
   * Prints a message to the homescreen.
   * @param s the string to print
   */
  def println(s: String) = {
    homeScn.text += s + "\n"
    homeScn.caret.dot = homeScn.text.length
  }
  val IDLE = 0
  val BUSY = 1
  val ASKING = 2
  var thread: ExecutionThread = null
  var msg: Any = null
  /**
   * The status.
   */
  var status = IDLE
  /**
   * Sets the status and repaints the status bar.
   * @param s the new status
   */
  def setSt(s: Int) = {
    status = s
    statusBar3.usb
    statusBar3.repaint
  }
  /**
   * Repaints the part of the status bar that shows the cd.
   */
  def updDirs = {
    statusBar1.usb
    statusBar1.repaint
  }
  val homeScn = new TextArea(20, 80) { // the middle panel, holding previous operations
    font = mono
    editable = false
    charWrap = true
    text = "Welcome to Amethyst " + Global.version + ".\n\n" +
      (if (mono.getFamily != "DejaVu Sans Mono") "Please install the DejaVu fonts.\n" else "")
    tooltip = "The homescreen. Holds previous operations and console output."
    lineWrap = true
  }
  val drawScn = new BufferedCanvas { // the top panel, for drawing
    preferredSize = new Dimension(640, 480)
    tooltip = "A canvas for graphical operations."
  }
  val statusBar1 = new Label {
    text = {
      "CUR:  " + Global.current + " (" + Global.currentAlias + ")"
    }
    font = mono
    horizontalAlignment = Alignment.Left
    def usb = {
      text = "CUR:  " + Global.current + " (" + Global.currentAlias + ")"
    }
  }
  val statusBar2 = new Label {
    text = {
      "ROOT: " + Global.root
    }
    font = mono
    horizontalAlignment = Alignment.Left
  }
  val statusBar3 = new Label {
    text = {
      Array[String]("IDLE", "BUSY", "ASKING").apply(status)
    }
    font = mono
    horizontalAlignment = Alignment.Left
    def usb = {
      text = Array[String]("IDLE", "BUSY", "ASKING").apply(status)
    }
  }
  /**
   * Inserts a string at the text caret.
   */
  def insertAtCaret(s: String) = {
    val curPos = inputArea.caret.dot
    inputArea.text = inputArea.text.substring(0, curPos) + s + inputArea.text.substring(curPos)
  }
  def enterEvent = {
    status match {
      case IDLE => runCodeConcurrently
      case ASKING => {
        msg = inputArea.text
        inputArea.text = ""
      }
    }
  }
  /**
   * Starts a new thread to run code.
   */
  def runCodeConcurrently() = {
    thread = new ExecutionThread
    thread.start
  }
  /**
   * Runs code.
   */
  def runCode() = {
    val toRun = inputArea.text
    if (toRun != "") {
      println(toRun)
      inputArea.text = ""
      setSt(BUSY)
      try {
        val bc = WholeParser.parse(toRun, p)
        val tp = Global.top
        tp.bytecode = bc
        tp.run
        println(tp.ans + "\n")
      } catch {
        case e: RuntimeException => {
          println(e.getMessage)
          System.out.println(e.getMessage)
          e.printStackTrace
        }
      }
      setSt(IDLE)
    }
  }
  val inputArea = new TextArea(5, 80) { // the bottom panel which holds the user's input
    font = mono
    tooltip = "Enter your code here!"
    lineWrap = true
  }
  def top = new MainFrame {
    Global.loadLib("std")
    p.loadOps
    title = "Amethyst " + Global.version

    val inputScroll = new ScrollPane(inputArea) {
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    }
    val homeScroll = new ScrollPane(homeScn) {
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    }
    val runButton = new Button {
      text = "Run (Ctrl + Enter)"
    }
    val lambdaButton = new Button {
      text = "λ"
    }
    val harpoonButton = new Button {
      text = "↼"
    }
    val superMinusButton = new Button {
      text = "⁻"
    }
    val buttons = new FlowPanel(lambdaButton, harpoonButton, superMinusButton, runButton)
    val statusBar = new BoxPanel(Orientation.Vertical) {
      contents += statusBar1
      contents += statusBar2
      contents += statusBar3
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= ArrayBuffer(drawScn, homeScroll, inputScroll, buttons, statusBar)
      focusable = true
      requestFocus
      listenTo(keys, drawScn.keys, homeScroll.keys, inputArea.keys, buttons.keys, lambdaButton, harpoonButton, runButton, superMinusButton)
      reactions += {
        case KeyPressed(_, Key.Enter, m, _) => {
          if ((m & 0xC0) != 0)
            enterEvent
        }
        case ButtonClicked(component) => {
          if (component == lambdaButton) insertAtCaret("λ")
          if (component == harpoonButton) insertAtCaret("↼")
          if (component == superMinusButton) insertAtCaret("⁻")
          if (component == runButton) enterEvent
        }
      }
    }
    size = new Dimension(640, 768)

  }
}

/**
 * An extension of a Scala Swing panel that holds a buffered image.
 */
class BufferedCanvas extends Panel { // sigh, I have to make one myself
  /**
   * The image.
   */
  var image = new BufferedImage(preferredSize.getWidth.toInt, preferredSize.getHeight.toInt, BufferedImage.TYPE_INT_ARGB)
  /**
   * The Graphics2D object associated with the image
   */
  val bg: Graphics2D = image.createGraphics
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.drawImage(image, 0, 0, null)
  }
  /**
   * Performs an action with the Graphics2D object.
   * @param grf a function that takes a Graphics2D and performs actions with it
   */
  def draw(grf: (Graphics2D) => Unit) = {
    grf(bg)
    repaint
  }
}

// argh, finally have to implement multithreading
/**
 * A thread object to run code.
 */
class ExecutionThread extends Thread {
  override def run = Main.runCode
}