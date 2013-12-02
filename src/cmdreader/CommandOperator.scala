package cmdreader

abstract class CommandOperator extends Command {
  require(isValidArg0(if (isUnary()) 1 else 2))
  def getOpAlias(): String = ""
  def isUnary(): Boolean = false
}