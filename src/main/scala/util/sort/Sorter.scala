package util.sort

import scala.collection.mutable._

/**
 * Functions for sorting based on a comparison function.
 * @author bluebear94
 */
object Sorter {
  /**
   * Performs heapsort.
   */
  def heapsort[T](a: ArrayBuffer[T], f: (T, T) => Boolean): ArrayBuffer[T] = {
    var b = a.clone
    var e = b.length - 1
    def swap(p: Int, q: Int) = {
      val t = b(p)
      b(p) = b(q)
      b(q) = t
    }
    def heapify = {
      var l = (e - 1) / 2
      while (l >= 0) {
        siftDown(l, e)
        l -= 1
      }
    }
    def siftDown(l: Int, r: Int): Unit = {
      var root = l
      while (2 * root + 1 <= r) {
        val child = 2 * root + 1
        var ts = root
        if (f(b(child), b(ts))) ts = child
        if (child + 1 <= r && f(b(child + 1), b(ts))) ts = child + 1
        if (ts != root) {
          swap(root, ts)
          root = ts
        }
        else return
      }
    }
    heapify
    while (e != 0) {
      swap(0, e)
      e -= 1
      siftDown(0, e)
    }
    b
  }
}