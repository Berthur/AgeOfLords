package mechanics.ui

import mechanics._
import GUI1._
import scala.swing._
import java.awt.Color


class AnnouncementFeed extends BoxPanel(Orientation.Vertical) {
  this.background = new Color(0xD1C099)
  val defaultColour = Color.BLACK
  this.foreground = defaultColour
  val defaultFont = new scala.swing.Font("andalus", 0, 18)
  this.font = defaultFont
  val prefix = "> "
  
  /**
   * Marker string, always to be on the last lines.
   * Since the last lines are sometimes not visible because of a long
   * announcement, this acts as a buffer to make sure the last relevant
   * item is always shown in the scrollpane.
   */
  val buffer1 = new TextArea {
    this.editable = false
    this.opaque = false
    this.font = defaultFont
    this.foreground = Color.BLUE
    this.rows = 10
    this.text = " . . . "
  }
  
  //Empty buffer lines:
  for (x <- 0 until 50) {
    contents += new TextArea {
      this.editable = false
      this.opaque = false
      this.font = defaultFont
      this.foreground = defaultColour
      this.text = ""
    }
  }
  
  /**
   * Announces a message with custom colour and font size.
   * NB! Large font sizes are not very well supported by the scrollpane's
   * mechanism to stay scrolled down, which may cause to ugly looking
   * half-announcements.
   */
  def announce(msg: String): Unit = announce(msg, defaultColour, defaultFont.getSize)
  
  /**
   * Announces a message with custom colour.
   */
  def announce(msg: String, colour: java.awt.Color): Unit = announce(msg, colour, defaultFont.getSize)
  
  /**
   * Announces a message to the player.
   */
  def announce(msg: String, colour: java.awt.Color, fontSize: Int): Unit = {
    this.contents -= buffer1
    this.contents += new TextArea {
      this.opaque = false
      this.editable = false
      this.lineWrap = true
      this.wordWrap = true
      this.font = new Font(defaultFont.getName, 0, fontSize)
      this.foreground = colour
      this.text = prefix + msg
    }

    this.contents += buffer1
    rightMenu.scrollDown()
  }
}
