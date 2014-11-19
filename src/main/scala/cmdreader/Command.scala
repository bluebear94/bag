package cmdreader
import types._
/**
 * A class to represent a preloaded command.
 * @author bluebear94
 */
abstract class Command {
  /**
   * Returns the name of the command.
   * @return the name of the command
   */
  def getName(): String// = ""
  /**
   * Returns whether a given arity is valid.
   * @param n the arity to check
   */
  def isValidArg0(n: Int): Boolean// = true
  /**
   * Returns the value returned by this command for a given set of arguments.
   * @param args the array containing the arguments
   */
  def apply(args: Array[Type]): Type// = TVoid
  /**
  * Returns whether this command always returns the same results for the same arguments,
  * and has no side effects.
  */
  def isPure: Boolean = false
  /**
  * Returns the protocol used by this command.
  */
  def protocol: FProtocol = FProtocol.empty
}
