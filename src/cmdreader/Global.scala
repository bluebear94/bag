package cmdreader

import scala.collection.mutable.HashMap

object Global {
  var liblist: HashMap[String, CmdList] = new HashMap[String, CmdList]()
  def loadLib(lname: String) = {
    liblist(lname) = new CmdList(lname)
  }
}