package gui

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.awt.{ Graphics2D, Color, Dimension }
import java.awt.image.BufferedImage
import cmdreader.Global
import parse.ast._
import scala.util.parsing.input.CharSequenceReader
import types.TVoid

// I'm a complete noob to this...

object Main extends SimpleSwingApplication {
  val mono = new Font("DejaVu Sans Mono", 0, 10)
  val p = new XprInt
  def top = new MainFrame {
    Global.loadLib("std")
    p.loadOps
    title = "Amethyst " + Global.version
    val inputArea = new TextArea(5, 80) { // the bottom panel which holds the user's input
      font = mono
      tooltip = "Enter your code here!"
      charWrap = true
    }
    val inputScroll = new ScrollPane(inputArea)
    val homeScn = new TextArea(20, 80) { // the middle panel, holding previous operations
      font = mono
      editable = false
      charWrap = true
      text = "Welcome to Amethyst " + Global.version + ".\n\n"
    }
    val homeScroll = new ScrollPane(homeScn)
    val drawScn = new BufferedCanvas { // the top panel, for drawing
      preferredSize = new Dimension(640, 480)
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
    val buttons = new FlowPanel(lambdaButton, harpoonButton, runButton)
    val inputAndButtons = new BorderPanel {
      layout(inputScroll) = Center
      layout(buttons) = South
    }
    contents = new BorderPanel {
      layout(drawScn) = North
      layout(homeScroll) = Center
      layout(inputAndButtons) = South
    }
    size = new Dimension(640, 768)
    listenTo(lambdaButton, harpoonButton, runButton)
    def insertAtCaret(s: String) = {
      val curPos = inputArea.caret.dot
      inputArea.text = inputArea.text.substring(0, curPos) + s + inputArea.text.substring(curPos)
    }
    def println(s: String) = {
      homeScn.text += s + "\n"
    }
    def runCode = {
      val toRun = inputArea.text
      println(toRun)
      inputArea.text = ""
      import p._
      val ast = p.expression(new p.PackratReader(new CharSequenceReader(toRun))) match {
        case Success(t, _) => {
          val tp = Global.top
          tp.bytecode = BFuncs.flatten(t.toBytecode) ++ Array[Byte](-0x17, 0x53)
          //try {
            tp.run
            println(tp.ans + "\n")
          //}
          //catch {
          //  case e: Exception => println(e.getMessage)
          //}
        }
        case NoSuccess(msg, _) => println(msg)
      }
    }
    reactions += {
      case ButtonClicked(component) => {
        if (component == lambdaButton) insertAtCaret("λ")
        if (component == harpoonButton) insertAtCaret("↼")
        if (component == runButton) runCode
      }
      case KeyPressed(inputArea, Key.Enter, Key.Control, Key.Location.Standard) => {
        runCode
      }
    }
  }
}

class BufferedCanvas extends Panel { // sigh, I have to make one myself
  var image = new BufferedImage(preferredSize.getWidth.toInt, preferredSize.getHeight.toInt, BufferedImage.TYPE_INT_ARGB)
  val bg: Graphics2D = image.createGraphics
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.drawImage(image, 0, 0, null)
  }
  def draw(grf: (Graphics2D) => Unit) = {
    grf(bg)
    repaint
  }
}