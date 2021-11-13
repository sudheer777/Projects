package sh.attempt.memory

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.Includes._
import scalafxml.core.{NoDependencyResolver, FXMLView, FXMLLoader}
import javafx.{scene => jfxs}
import scalafx.collections.ObservableBuffer
import sh.attempt.memory.model.Note
import sh.attempt.memory.view.ModifyNote
import sh.attempt.memory.view.CreateNote
import scalafx.stage.Stage
import scalafx.stage.Modality
import scalafx.scene.image.Image
import sh.attempt.memory.util.Database

object MainApp extends JFXApp {
  Database.setupDB()

  val noteData = new ObservableBuffer[Note]()

  noteData ++= Note.getAllNotes

  val rootResource = getClass.getResource("view/Layout.fxml")
  val loader = new FXMLLoader(rootResource, NoDependencyResolver)
  loader.load();
  val roots = loader.getRoot[jfxs.layout.BorderPane]

  stage = new PrimaryStage {
    title = "Quartz Note"
    icons += new Image(getClass().getResourceAsStream("images/NoteBook.png"))
    scene = new Scene {
      root = roots
    }
  }

  def showMainNoteWindow() = {
    val resource = getClass.getResource("view/MainNoteWindow.fxml")
    val loader = new FXMLLoader(resource, NoDependencyResolver)
    loader.load();
    val roots = loader.getRoot[jfxs.layout.AnchorPane]
    this.roots.setCenter(roots)
  } 

  showMainNoteWindow()

  def showModifyNoteWindow(note: Note): Boolean = {
    val resource = getClass.getResource("view/ModifyNoteWindow.fxml")
    val loader = new FXMLLoader(resource, NoDependencyResolver)
    loader.load();
    val roots2  = loader.getRoot[jfxs.Parent]
    val control = loader.getController[ModifyNote#Controller]

    val dialog = new Stage() {
      initModality(Modality.APPLICATION_MODAL)
      initOwner(stage)
      title = "Modify Note"
      icons += new Image(getClass().getResourceAsStream("images/NoteBook.png"))
      scene = new Scene {
        root = roots2
      }
    }
    control.dialogStage = dialog
    control.note = note
    dialog.showAndWait()
    control.updateClicked
  } 

  def showCreateNoteWindow(note: Note): Boolean = {
    val resource = getClass.getResource("view/CreateNoteWindow.fxml")
    val loader = new FXMLLoader(resource, NoDependencyResolver)
    loader.load();
    val roots2  = loader.getRoot[jfxs.Parent]
    val control = loader.getController[CreateNote#Controller]

    val dialog = new Stage() {
      initModality(Modality.APPLICATION_MODAL)
      initOwner(stage)
      title = "Create Note"
      icons += new Image(getClass().getResourceAsStream("images/NoteBook.png"))
      scene = new Scene {
        root = roots2
      }
    }
    control.dialogStage = dialog
    control.note = note
    dialog.showAndWait()
    control.saveClicked
  } 
}
