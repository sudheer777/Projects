package sh.attempt.memory.model

import scalafx.beans.property.{StringProperty, ObjectProperty}
import java.time.LocalDate
import sh.attempt.memory.util.Database
import sh.attempt.memory.util.Date._
import scalikejdbc._
import scala.util.{ Try, Success, Failure }

class Note (val purposeS : String, val titleS : String) extends Database {
	def this()     = this(null, null)
	var purpose    = new StringProperty(purposeS) 
	var title      = new StringProperty(titleS) 
  	var entry      = new StringProperty("")
	var date       = new ObjectProperty[LocalDate]


	def save() : Try[Int] = {
    if (!(isExist)) {
      Try(DB autoCommit { implicit session => 
      sql"""
				insert into note (purpose, title,
				entry, date) values 
				(${purpose.value}, ${title.value}, 
				 ${entry.value}, ${date.value.asString})
			""".update.apply()
			})

		} else {
			Try(DB autoCommit { implicit session => 
			sql"""
				update note 
				set 
				purpose  = ${purpose.value} ,
				title    = ${title.value},
				entry    = ${entry.value},
				date     = ${date.value.asString}
				where purpose = ${purpose.value} and
				title = ${title.value}
			""".update.apply()
			})
		}	
	}

	def update() : Try[Int] = {
    if (!(isExist)) {
      Try(DB autoCommit { implicit session => 
      sql"""
				insert into note (purpose, title,
				entry, date) values 
				(${purpose.value}, ${title.value}, 
				 ${entry.value}, ${date.value.asString})
			""".update.apply()
			})

		} else {
			Try(DB autoCommit { implicit session => 
			sql"""
				update note 
				set 
				purpose  = ${purpose.value} ,
				title    = ${title.value},
				entry    = ${entry.value},
				date     = ${date.value.asString}
				where purpose = ${purpose.value} and
				title = ${title.value}
			""".update.apply()
			})
		}	
	}

	def delete() : Try[Int] = {
		if (isExist) {
			Try(DB autoCommit { implicit session => 
			sql"""
				delete from note where  
				purpose = ${purpose.value} and title = ${title.value}
			""".update.apply()
			})
		} else 
			throw new Exception("This note does not exist in the database.")		
	}

	def isExist : Boolean =  {
		DB readOnly { implicit session =>
			sql"""
				select * from note where 
				purpose = ${purpose.value} and title = ${title.value}
			""".map(rs => rs.string("purpose")).single.apply()
		} match {
			case Some(x) => true
			case None => false
		}

	}
}

object Note extends Database{
	def apply (
		purposeS : String, 
		titleS   : String,
		entryS   : String,
		dateS    : String
		) : Note = {

		new Note(purposeS, titleS) {
			entry.value     = entryS
			date.value      = dateS.parseLocalDate
		}
		
	}
	def initializeTable() = {
		DB autoCommit { implicit session => 
			sql"""
			create table note (
			  id int not null GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1), 
			  purpose varchar(64), 
			  title varchar(64), 
			  entry varchar(600),
			  date varchar(64)
			)
			""".execute.apply()
		}
	}
	
	def getAllNotes : List[Note] = {
		DB readOnly { implicit session =>
			sql"select * from note".map(rs => Note(rs.string("purpose"),
				rs.string("title"),rs.string("entry"), 
				rs.string("date") )).list.apply()
		}
	}
}
