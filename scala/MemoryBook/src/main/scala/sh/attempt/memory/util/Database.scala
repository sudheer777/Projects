package sh.attempt.memory.util

import scalikejdbc._
import sh.attempt.memory.model.Note
trait Database {
  val derbyDriverClassname = "org.apache.derby.jdbc.EmbeddedDriver"

  val dbURL = "jdbc:derby:myDB;create=true;";
  Class.forName(derbyDriverClassname)
  
  ConnectionPool.singleton(dbURL, "sarahopuimun", "18026666")

  implicit val session = AutoSession
}

object Database extends Database{
  def setupDB() = {
  	if (!hasDBInitialize)
  		Note.initializeTable()
  }

  def hasDBInitialize : Boolean = {
    DB getTable "Note" match {
      case Some(x) => true
      case None => false
    }
  }
}
