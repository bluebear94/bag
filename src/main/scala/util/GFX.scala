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
    for (x <- x0 until x1) {
      for (y <- y0 until y1) {
        val c = f(x, y)
        setcol(new Color(if (alpha) c else (c | 0xFF000000)))
        ptOn(x, y)
      }
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
  def test() = { // I had this planned since the birth of the gfx library. Mwahahahaha!
    var prev = false
    def nb = {
      prev = if (Global.r.nextDouble > 0.8) Global.r.nextBoolean else !prev
      prev
    }
    def ttf = 3 + Global.r.nextInt(3)
    def kagome(x0: Int, x1: Int, y0: Int, y1: Int): Unit = {
      def dot(x: Int, y: Int) = circ(x * 16, y * 16, 4, true)
      val vert = nb
      if (if (vert) Math.abs(x0 - x1) > ttf else Math.abs(y0 - y1) > ttf) {
        val pct = Global.r.nextDouble * 0.4 + 0.3
        val dx = x1 - x0
        val dy = y1 - y0
        val sp = (if (vert) x0 + pct * dx else y0 + pct * dy).toInt
        if (vert) {
          kagome(x0, sp, y0, y1)
          kagome(sp + 1, x1, y0, y1)
          for (i <- y0 to y1)
            dot(sp, i)
        } else {
          kagome(x0, x1, y0, sp)
          kagome(x0, x1, sp + 1, y1)
          for (i <- x0 to x1)
            dot(i, sp)
        }
      }
    }
    setcol(Color.GREEN)
    kagome(0, 40, 0, 30)
    setcol(Color.WHITE)
    text("\u30ab\u30b4\u30e1\u30ab\u30b4\u30e1", 550, 20)
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
