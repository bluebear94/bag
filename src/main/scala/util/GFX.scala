package util

import gui._
import java.awt._
import types._
import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Field

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
  def line(x0: Int, y0: Int, x1: Int, y1: Int) = {
    ds.draw(_.drawLine(x0, y0, x1, y1))
  }
  def rect(x0: Int, y0: Int, x1: Int, y1: Int, f: Boolean) = {
    if (f) ds.draw(_.fillRect(x0, y0, x1 - x0, y1 - y0))
    else ds.draw(_.drawRect(x0, y0, x1 - x0, y1 - y0))
  }
  def circ(x: Int, y: Int, r: Int, f: Boolean) = {
    if (f) ds.draw(_.fillOval(x - r, y - r, r << 1, r << 1))
    else ds.draw(_.drawOval(x - r, y - r, r << 1, r << 1))
  }
  def ell(x0: Int, y0: Int, x1: Int, y1: Int, f: Boolean) {
    if (f) ds.draw(_.fillOval(x0, y0, x1 - x0, y1 - y0))
    else ds.draw(_.drawOval(x0, y0, x1 - x0, y1 - y0))
  }
  def ctd(xs: Array[Int], ys: Array[Int]) = {
    ds.draw(_.drawPolyline(xs, ys, xs.length))
  }
  def poly(xs: Array[Int], ys: Array[Int], f: Boolean) = {
    if (f) ds.draw(_.fillPolygon(xs, ys, xs.length))
    else ds.draw(_.drawPolygon(xs, ys, xs.length))
  }
  def getf = {
    ds.bg.getFont
  }
  def setf(f: Font) = {
    ds.draw(_.setFont(f))
  }
  def text(s: String, x: Int, y: Int) = {
    ds.draw(_.drawString(s, x, y))
  }
  def serializeFont(f: Font): LArray = {
    new LArray(ArrayBuffer(TString(f.getName), TMountain(f.getStyle), TMountain(f.getSize)))
  }
  def deserializeFont(f: LArray): Font = {
    val z = f.l(0)
    val o = f.l(1)
    val t = f.l(2)
    new Font(z.toString, getIntOrChoke(o), getIntOrChoke(t))
  }
  def test() = {
    setcol(Color.YELLOW)
    circ(320, 240, 200, true)
    setcol(Color.BLACK)
    circ(200, 200, 20, true)
    circ(440, 200, 20, true)
    line(200, 300, 320, 320)
    line(440, 300, 320, 320)
  }
  /**
   * Method to return a color by name.
   * @param n the color name
   * @return the color, or black if not recognized
   */
  def stc(n: String) = {
    try {
      val f = Class.forName("java.awt.Color").getField(n)
      f.get(null).asInstanceOf[Color]
    }
    catch {
      case e: Exception => Color.BLACK
    }
  }
  def getIntOrChoke(t: Type) = {
    t match {
      case n: TNumerical => n.intValue
      case _ => throw new IllegalArgumentException
    }
  }
}