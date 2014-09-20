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
import run.ISwitch


// I'm <u>kind of</u> a <s>complete</s> noob to this...

/**
 * The graphic user interface for the language.
 * @author bluebear94
 */
object Main extends SimpleSwingApplication {
  /**
   * Standard monospace font.
   */
  val mono = new Font(Font.MONOSPACED, 0, 10)
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
  def clrHome() = {
    homeScn.text = ""
    homeScn.caret.dot = 0
  }
  val IDLE = 0
  val BUSY = 1
  val ASKING = 2
  var thread: ExecutionThread = null
  val switch: ISwitch = new ISwitch
  var msg: String = null
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
    text = "Welcome to Bag " + Global.version + ".\nEnter `$:help()' to get help.\n\n"// +
      //(if (mono.getFamily != "DejaVu Sans Mono") "Please install the DejaVu fonts.\n" else "")
    tooltip = "The homescreen. Holds previous operations and console output."
    lineWrap = true
  }
  val drawScn = new BufferedCanvas(new Dimension(640, 480)) { // the top panel, for drawing
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
  val statusBar4 = new Label {
    text = {
      val r = Runtime.getRuntime
      s"Memory (F/T/M): ${r.freeMemory}/${r.totalMemory}/${r.maxMemory}"
    }
    font = mono
    horizontalAlignment = Alignment.Left
    override def repaint() = {
      val r = Runtime.getRuntime
      text = s"Memory (F/T/M): ${r.freeMemory}/${r.totalMemory}/${r.maxMemory}"
      super.repaint()
    }
  }
  val mmt = new MemoryManagerThread
  mmt.setPriority(2)
  mmt.start()
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
  def stopEvent = switch.trigger()
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
        tp.run(Main.switch)
        println((if (toRun == "3-1+.5-2") "There are no buses in Gensokyo!" else tp.ans) + "\n")
      } catch {
        case e: RuntimeException => {
          println(e.getMessage)
          System.out.println(e.getMessage)
          e.printStackTrace
        }
      }
      setSt(IDLE)
      switch.reset()
    }
  }
  def ask = {
    Main.msg = ""
    Main.setSt(Main.ASKING)
    while (Main.msg.isEmpty) Thread.sleep(10)
    Main.setSt(Main.BUSY)
    Main.println(msg)
    msg
  }
  val inputArea = new TextArea(5, 80) { // the bottom panel which holds the user's input
    font = mono
    tooltip = "Enter your code here!"
    lineWrap = true
  }
  def top = new MainFrame {
    Global.loadLib("std")
    p.loadOps
    title = "Bag " + Global.version

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
    val rightArrowButton = new Button {
      text = "→"
    }
    val stopButton = new Button {
      text = "Stop (Ctrl + Z)"
      foreground = Color.RED
    }
    val buttons = new FlowPanel(lambdaButton, harpoonButton, superMinusButton, rightArrowButton, runButton, stopButton)
    val statusBar = new BoxPanel(Orientation.Vertical) {
      contents += statusBar1
      contents += statusBar2
      contents += statusBar3
      contents += statusBar4
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= ArrayBuffer(drawScn, homeScroll, inputScroll, buttons, statusBar)
      focusable = true
      requestFocus
      listenTo(keys, drawScn.keys, homeScroll.keys, inputArea.keys, buttons.keys, lambdaButton,
          harpoonButton, runButton, superMinusButton, rightArrowButton)
      def isCtrl(m: Int) = (m & 0xC0) != 0
      reactions += {
        case KeyPressed(_, Key.Enter, m, _) => {
          if (isCtrl(m))
            enterEvent
        }
        case KeyPressed(_, Key.Z, m, _) => {
          if (isCtrl(m))
            stopEvent
        }
        case ButtonClicked(component) => {
          if (component == lambdaButton) insertAtCaret("λ")
          if (component == harpoonButton) insertAtCaret("↼")
          if (component == superMinusButton) insertAtCaret("⁻")
          if (component == rightArrowButton) insertAtCaret("→")
          if (component == runButton) enterEvent
          if (component == stopButton) stopEvent
        }
      }
    }
    size = new Dimension(640, 768)

  }
}

/**
 * An extension of a Scala Swing panel that holds a buffered image.
 * @param d The size of the image
 */
class BufferedCanvas(d: Dimension) extends Panel { // sigh, I have to make one myself
  preferredSize = d
  /**
   * The image.
   */
  val image = new BufferedImage(preferredSize.width, preferredSize.height, BufferedImage.TYPE_INT_ARGB)
  //println(preferredSize)
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
    //println(preferredSize)
  }
}

// argh, finally have to implement multithreading
/**
 * A thread object to run code.
 */
class ExecutionThread extends Thread {
  override def run = Main.runCode
}
/**
  A thread that updates Status Bar #4, which holds memory info.
*/
class MemoryManagerThread extends Thread {
  override def run = {
    while (true) {
      Main.statusBar4.repaint()
      Thread.sleep(1000)
    }
  }
}
