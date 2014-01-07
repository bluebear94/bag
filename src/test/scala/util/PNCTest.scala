package util
import org.scalatest._
import util._
class PathnameParsersTest extends FlatSpec with ShouldMatchers {
  "osToA" should "convert OS pathnames to their Amethyst counterparts" in {
    import PathNameConverter._
    osToA("ex.variable") should equal("ex")
    osToA("exf/ex.variable") should equal("exf.ex")
    osToA("math/homework1/a.variable") should equal("math.homework1.a")
  }
  "aToOs" should "convert Amethyst pathnames to their Amethyst counterparts" in {
    import PathNameConverter._
    aToOs("ex", false) should equal(("ex.variable", false))
    aToOs("exf.ex", false) should equal(("exf/ex.variable", false))
    aToOs("root.math.homework1.a", false) should equal(("math/homework1/a.variable", true))
  }
}