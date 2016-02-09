package mechanics.ui

import scala.swing._
import GUI1._

/**
 * Represents a specific type of simple dialog window, containing a title, text and
 * an arbitrary number of buttons with their respective actions. The dialog is closed
 * if any button is pressed.
 * @param dialogTitle The title of the dialog.
 * @param msg: The text that is displayed in the dialog.
 * @param buttons: An arbitrary number of (button text, button action) pairs.
 */
class GameDialog(dialogTitle: String, msg: String, buttons: (String, () => Unit)*) extends Dialog {
  this.centerOnScreen()
  title = dialogTitle
  contents = new BorderPanel {
    layout(new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(5,5,5,5)
      contents += new Label(msg)
    }) = BorderPanel.Position.Center
    layout(new FlowPanel(FlowPanel.Alignment.Right)(
        buttons.map(button => Button(button._1) {button._2.apply(); close()}):_*
        )) = BorderPanel.Position.South
  }
}