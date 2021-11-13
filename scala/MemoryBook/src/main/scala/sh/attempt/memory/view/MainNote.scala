package sh.attempt.memory.view

import sh.attempt.memory.model.Note
import sh.attempt.memory.MainApp
import scalafx.scene.control.{TableView, TableColumn, Label}
import scalafxml.core.macros.sfxml
import scalafx.beans.property.{StringProperty} 
import sh.attempt.memory.util.Date._
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scala.util.{Success,Failure}

@sfxml
class MainNote(
    private val noteTable : TableView[Note],
    private val purposeColumn : TableColumn[Note, String],
    private val titleColumn : TableColumn[Note, String],
    private val purposeLabel : Label,
    private val titleLabel : Label,
    private val entryLabel : Label,
    private val recordLabel : Label  
    ) {
  noteTable.items = MainApp.noteData
  purposeColumn.cellValueFactory = {_.value.purpose}
  titleColumn.cellValueFactory  = {_.value.title} 
  
  showNoteDetails(None);
  
  noteTable.selectionModel().selectedItem.onChange(
      (_, _, newValue) => showNoteDetails(Option(newValue))
  )


    private def showNoteDetails (note : Option[Note]) = {
    note match {
      case Some(note) =>
      purposeLabel.text <== note.purpose
      titleLabel.text   <== note.title
      entryLabel.text   <== note.entry
      recordLabel.text  = note.date.value.asString
      
      
      case None =>
      purposeLabel.text.unbind()
      titleLabel.text.unbind()
      entryLabel.text.unbind()
      purposeLabel.text = ""
      titleLabel.text   = ""
      entryLabel.text   = ""
      recordLabel.text  = ""
    }    
  }

 
  def handleDeleteNote(action : ActionEvent) = {
    val selectedIndex = noteTable.selectionModel().selectedIndex.value
    val selectedNote = noteTable.selectionModel().selectedItem.value
    if (selectedIndex >= 0) {
      selectedNote.delete() match {
        case Success(x) => 
        MainApp.noteData.remove(selectedIndex);
        case Failure(e) => 
        val alert = new Alert(AlertType.Error){
          initOwner(MainApp.stage)
          title       = "The note was not deleted"
          headerText  = "Error(s) in the database"
          contentText = "Due to error(s) in the database, the note was not deleted."
        }.showAndWait()
      }
        
    } else {
        val alert = new Alert(AlertType.Warning){
          initOwner(MainApp.stage)
          title       = "Warning"
          headerText  = "No Note Chosen to Delete"
          contentText = "Please choose a note in the table to delete."
        }.showAndWait()
    }

  } 

  def handleCreateNote(action : ActionEvent) = {
    val note = new Note("","")
    val saveClicked = MainApp.showCreateNoteWindow(note);
        if (saveClicked) {
          note.save() match{
            case Success(value) =>
              MainApp.noteData += note
            case Failure(exception) =>
            val alert = new Alert(Alert.AlertType.Error){
              initOwner(MainApp.stage)
              title       = "The note was not saved"
              headerText  = "Error(s) in the database"
              contentText = "Due to error(s) in the database, the note was not saved."
            }.showAndWait()
          }
        }
  }

  def handleModifyNote(action : ActionEvent) = {
    val selectedNote = noteTable.selectionModel().selectedItem.value
    if (selectedNote != null) {
        val updateClicked = MainApp.showModifyNoteWindow(selectedNote)

        if (updateClicked) {
          selectedNote.update() match {
            case Success(value) => showNoteDetails(Some(selectedNote))
            case Failure(exception) => 
            val alert = new Alert(Alert.AlertType.Error){
            initOwner(MainApp.stage)
            title       = "The note was not updated"
            headerText  = "Error(s) in the database"
            contentText = "Due to error(s) in the database, the changes made to the note was not updated."
          }.showAndWait()
        }
      }

    } else {
        val alert = new Alert(Alert.AlertType.Warning){
          initOwner(MainApp.stage)
          title       = "Warning"
          headerText  = "No Note Chosen for Modification"
          contentText = "Please choose a note in the table to modify."
        }.showAndWait()
    }
  } 
}
