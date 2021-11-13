package sh.attempt.memory.view

import sh.attempt.memory.model.Note
import sh.attempt.memory.MainApp
import scalafx.scene.control.{TextField, TextArea, TableColumn, Label, Alert}
import scalafxml.core.macros.sfxml
import scalafx.stage.Stage
import scalafx.Includes._
import sh.attempt.memory.util.Date._
import scalafx.event.ActionEvent

@sfxml
class ModifyNote (

    private val  purposeField : TextField,
    private val    titleField : TextField,
    private val     entryArea : TextArea,
    private val   recordField : TextField

){
  var         dialogStage : Stage  = null
  private var _note       : Note   = null
  var         updateClicked        = false

  def note = _note
  def note_=(x : Note) {
        _note = x

        purposeField.text = _note.purpose.value
        titleField.text   = _note.title.value
        entryArea.text    = _note.entry.value
        recordField.text  = _note.date.value.asString
  }

  def handleUpdate(action :ActionEvent){
    if (isInputValid()) {
      _note.purpose      <== purposeField.text
      _note.title        <== titleField.text
      _note.entry        <== entryArea.text
      _note.date.value   = recordField.text.value.parseLocalDate;

      updateClicked = true;
      dialogStage.close()
    }
  }

  def handleCancel(action :ActionEvent) {
    dialogStage.close();
  }

  def nullChecking (x : String) = x == null || x.length == 0

  def isInputValid() : Boolean = {
    var errorMessage = ""

    if (nullChecking(purposeField.text.value))
      errorMessage += "No purpose has been entered.\n"
    if (nullChecking(titleField.text.value))
      errorMessage += "No title has been entered.\n"
    if (nullChecking(entryArea.text.value))
      errorMessage += "No entry notes has been entered.\n"
    if (nullChecking(recordField.text.value))
      errorMessage += "No record date has been entered.\n"
    else {
      if (!recordField.text.value.isValid) {
        errorMessage += "The format dd/mm/yyyy is to be used for a valid record date.\n";
      }
    }

    if (errorMessage.length() == 0) {
        return true;
    } else {
        val alert = new Alert(Alert.AlertType.Error){
          initOwner(dialogStage)
          title = "Error"
          headerText = "Empty/Incomplete/Incorrect Information in respective fields"
          contentText = errorMessage
        }.showAndWait()

        return false;
    }
   }
} 
