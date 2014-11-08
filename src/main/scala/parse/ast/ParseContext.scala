package parse.ast

import types._
import cmdreader._
import run._
import scala.collection.mutable.HashMap // I'm sorry.

class ParseContext extends HasEnvironment {
  var environment: HashMap[String, Type] = HashMap(
    "PI" -> TFish(Math.PI),
    "TAU" -> TFish(2 * Math.PI),
    "E" -> TFish(Math.E)
  )
  def addConst(name: String, value: Type) = {
    if (environment.isDefinedAt(name)) throw new RuntimeException(s"Constant $name is already defined as $value")
    else environment(name) = value
  }
  def const(name: String) = environment(name)
  def calling = null
  def fname = ""
  var args = Array[Type]()
  def toRI: RunningInstance = {
    val ri = new RunningInstance("", null, Array[Type]())
    ri.environment = this.environment
    ri
  }
}
