package util

import gui._
import java.awt._
import types._
import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Field
import cmdreader.Global

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
  def ptOn(x: Int, y: Int) = {
    ds.draw(_.fillRect(x, y, 1, 1))
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
  def iterate(f: (Int, Int) => Int, alpha: Boolean, x0: Int, y0: Int, x1: Int, y1: Int) = {
    var x = x0
    while (x < x1) {
      var y = y0
      while (y < y1) {
        val c = f(x, y)
        setcol(new Color(if (alpha) c else (c | 0xFF000000)))
        ptOn(x, y)
        y += 1
      }
      x += 1
    }
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
  def fromHSV(h: Double, s: Double, v: Double): Int = {
    // see http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
    val SIXTY_DEGREES = Math.PI / 3
    val c = v * s
    val x = c * (1 - Math.abs((h / SIXTY_DEGREES) % 2.0 - 1))
    val m = v - c
    val (rp: Double, gp: Double, bp: Double) =
      (h / SIXTY_DEGREES).toInt match {
        case 0 => (c, x, 0.0)
        case 1 => (x, c, 0.0)
        case 2 => (0.0, c, x)
        case 3 => (0.0, x, c)
        case 4 => (x, 0.0, c)
        case 5 => (c, 0.0, x)
      }
    val (r, g, b) = (rp + m, gp + m, bp + m)
    val (ri, gi, bi) = ((255 * r).toInt, (255 * g).toInt, (255 * b).toInt)
    (ri << 16) + (gi << 8) + bi
  }
  def test() = { // I had this planned since the birth of the gfx library. Mwahahahaha!
    iterate((x, y) => {
      val yp = y - 120
      if (x > 120 || (Math.pow(x - 120, 2) + Math.pow(yp - 120, 2) < 14400)) {
        val h = ((y - (x >> 2)) % 360).toRadians
        fromHSV(h, 0.3, 1.0)
      } else 0
    }, false, 0, 120, 640, 360)
    val old = getf
    setf(new Font("sans-serif", 0, 40))
    text("Love Sign - \"Master Spark\"", 100, 100)
    setf(old)
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
      case e: Exception => {
        try {
          Color.decode(n)
        }
        catch {
          case e: NumberFormatException => Color.BLACK
        }
      }
    }
  }
  def getIntOrChoke(t: Type) = {
    t match {
      case n: TNumerical => n.intValue
      case _ => throw new IllegalArgumentException
    }
  }
}
