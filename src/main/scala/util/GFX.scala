package util

import gui._
import java.awt._

object GFX {
  lazy val ds = Main.drawScn
  def clrscn() = {
    ds.draw(_.clearRect(0, 0, 640, 480))
  }
  def setcol(c: Color) = {
    ds.draw(_.setColor(c))
  }
  def getcol = {
    ds.bg.getColor
  }
}