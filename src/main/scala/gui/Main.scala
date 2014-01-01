package gui

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.awt.{ Graphics2D, Color, Dimension }
import java.awt.image.BufferedImage
import cmdreader.Global

// I'm a complete noob to this...

object Main extends SimpleSwingApplication {
  val mono = new Font("DejaVu Sans Mono", 0, 10)
  def top = new MainFrame {
    title = "Amethyst " + Global.version
    val inputArea = new ScrollPane(new TextArea(5, 80) { // the bottom panel which holds the user's input
      font = mono
      tooltip = "Enter your code here!"
      charWrap = true
    })
    val homeScn = new ScrollPane(new TextArea(20, 80) { // the middle panel, holding previous operations
      font = mono
      editable = false
      charWrap = true
      text = "Welcome to Amethyst " + Global.version + "."
    })
    val drawScn = new BufferedCanvas { // the top panel, for drawing
      preferredSize = new Dimension(640, 480)
    }
    val runButton = new Button {
      text = "Run"
    }
    val buttons = new FlowPanel(runButton)
    contents = new BorderPanel {
      layout(drawScn) = North
      layout(homeScn) = Center
      layout(inputArea) = South
    }
    size = new Dimension(640, 768)
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