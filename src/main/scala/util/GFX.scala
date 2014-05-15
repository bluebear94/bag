package util

import gui._
import java.awt._
import types._
import scala.collection.mutable.ArrayBuffer

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
    if (f) ds.draw(_.fillRect(x0, y0, x1, y1))
    else ds.draw(_.drawRect(x0, y0, x1, y1))
  }
  def circ(x: Int, y: Int, r: Int, f: Boolean) = {
    if (f) ds.draw(_.fillOval(x - r, y - r, r << 1, r << 1))
    else ds.draw(_.drawOval(x - r, y - r, r << 1, r << 1))
  }
  def ell(x0: Int, y0: Int, x1: Int, y1: Int, f: Boolean) {
    if (f) ds.draw(_.fillOval(x0, y0, x1, y1))
    else ds.draw(_.drawOval(x0, y0, x1, y1))
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
    new Font(z.toString, o match {
      case o: TNumerical => o.intValue
      case _ => throw new IllegalArgumentException
    }, t match {
      case t: TNumerical => t.intValue
      case _ => throw new IllegalArgumentException
    })
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
}