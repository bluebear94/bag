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
  def println(s: String) = {
    homeScn.text += s + "\n"
  }
  val IDLE = 0
  val BUSY = 1
  val ASKING = 2
  var status = IDLE
  val homeScn = new TextArea(20, 80) { // the middle panel, holding previous operations
    font = mono
    editable = false
    charWrap = true
    text = "Welcome to Amethyst " + Global.version + ".\n\n"
  }
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
      focusable = true
      requestFocus
      listenTo(keys, drawScn.keys, homeScroll.keys, inputArea.keys, buttons.keys, lambdaButton, harpoonButton, runButton)
      reactions += {
        case KeyPressed(_, Key.Enter, m, _) => {
          if ((m & 0xC0) != 0)
            runCode()
        }
        case ButtonClicked(component) => {
          if (component == lambdaButton) insertAtCaret("λ")
          if (component == harpoonButton) insertAtCaret("↼")
          if (component == runButton) runCode()
        }
      }
    }
    size = new Dimension(640, 768)
    def insertAtCaret(s: String) = {
      val curPos = inputArea.caret.dot
      inputArea.text = inputArea.text.substring(0, curPos) + s + inputArea.text.substring(curPos)
    }
    def runCode() = {
      val toRun = inputArea.text
      if (toRun != "") {
        println(toRun)
        inputArea.text = ""
        status = BUSY
        val bc = WholeParser.parse(toRun, p)
        val tp = Global.top
        tp.bytecode = bc
        tp.run
        status = IDLE
        println(tp.ans + "\n")
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