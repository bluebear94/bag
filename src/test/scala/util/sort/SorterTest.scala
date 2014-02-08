package util.sort
import org.scalatest._
import util.sort._
import scala.collection.mutable._
class SorterTest extends FlatSpec with ShouldMatchers {
  "heapsort" should "perform heapsort" in {
    import util.sort.Sorter._
    heapsort(ArrayBuffer(7,3,8,5,1,2,6,9), (a: Int, b: Int) => a > b) should equal(ArrayBuffer(1,2,3,5,6,7,8,9))
  }
  "quicksort" should "perform quicksort" in {
    import util.sort.Sorter._
    quicksort(ArrayBuffer(7,3,8,5,1,2,6,9), (a: Int, b: Int) => a > b) should equal(ArrayBuffer(1,2,3,5,6,7,8,9))
  }
}