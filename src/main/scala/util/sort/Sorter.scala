package util.sort

import scala.collection.mutable._
import cmdreader._

/**
 * Functions for sorting based on a comparison function.
 * @author bluebear94
 */
object Sorter {
  /**
   * Performs heapsort.
   * @param a the <code>ArrayBuffer</code> to sort
   * @param f the comparison function. For all <code>i &gt; j</code>, <code>f(a(i), a(j))</code> after sorting.
   * @return the sorted <code>ArrayBuffer</code>
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
        } else return
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
  /**
   * Performs randomized quicksort.
   * @param a the <code>ArrayBuffer</code> to sort
   * @param f the comparison function. For all <code>i &gt; j</code>, <code>f(a(i), a(j))</code> after sorting.
   * @return the sorted <code>ArrayBuffer</code>
   */
  def quicksort[T](a: ArrayBuffer[T], f: (T, T) => Boolean): ArrayBuffer[T] = {
    var b = a.clone
    val e = b.length - 1
    def swap(p: Int, q: Int) = {
      val t = b(p)
      b(p) = b(q)
      b(q) = t
    }
    def qsortr(i: Int, j: Int) { // NOTE! Includes j!
      if (j - i > 0) {
        val k = Global.r.nextInt(j - i + 1) + i
        val p = b(k)
        var ii = i
        var jj = j
        while (jj - ii > 0) {
          if (f(b(ii), p)) {
            swap(ii, jj)
            jj -= 1
          } else if (f(p, b(jj))) {
            swap(ii, jj)
            ii += 1
          } else {
            ii += 1
            jj -= 1
          }
        }
        val m = Math.min(Math.max(ii, i + 1), j - 1)
        if (j - i > 1) {
          qsortr(i, m)
          qsortr(m, j)
        }
      }
    }
    qsortr(0, e)
    b
  }
  /**
   * Performs mergesort. (<a href="http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html">algorithm</a>)
   * @param a the <code>ListBuffer</code> to sort
   * @param f the comparison function. For all <code>i &gt; j</code>, <code>f(a(i), a(j))</code> after sorting.
   * @return the sorted <code>ListBuffer</code>
   */
  def mergesort[T](a: ListBuffer[T], f: (T, T) => Boolean): ListBuffer[T] = {
    var b = a
    var k = 1
    var isDone = false
    var merges = 99
    while (merges != 1) {
      var p = b
      b = new ListBuffer[T]()
      merges = 0
      while (!p.isEmpty) {
        var q = p
        var psize = 0
        while (psize < k && !q.isEmpty) {
          q = q.tail
          psize += 1
        }
        var qsize = k
        while (psize > 0 || (qsize > 0 && !q.isEmpty)) {
          val useP = {
            if (q.isEmpty || qsize == 0) true
            else if (psize == 0) false
            else {
              val ph: T = p.head
              val qh: T = q.head
              val pig = f(ph, qh)
              val qig = f(qh, ph)
              if (pig == qig) true
              else qig
            }
          }
          if (useP) {
            b += p.head
            p = p.tail
            psize -= 1
          } else {
            b += q.head
            q = q.tail
            qsize -= 1
          }
        }
        p = q
        merges += 1
      }
      k *= 2
    }
    b
  }
}