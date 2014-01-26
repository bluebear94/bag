package util
import types._

object TurnOnSide {
  // Trying to write a generalized function drove me crazy.
  def apply(mat: Array[List[Type]]): List[Array[Type]] = {
    mat(0) match {
      case Nil => Nil
      case _ => {
        mat.map(_.head) :: apply(mat.map(_.tail))
      }
    }
  }
}