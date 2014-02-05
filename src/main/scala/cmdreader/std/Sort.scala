package cmdreader.std
import types._
import cmdreader._
/**
 * A class to represent a sorting command.
 * @author bluebear94
 */
abstract class Sort extends CommandOperator {
  override def isValidArg0(n: Int): Boolean = n == 2
  def getPrecedence(): Int = PStandard.MAP
  def isReversed(): Boolean = false
  def hasAssignmentEquiv(): Boolean = true
  def getDoubleBase(): Option[Type] = None
}