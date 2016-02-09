package mechanics.ui

import java.io.File
import javax.sound.sampled._

object Audio {
  
  /**
   * "Cobblestone village", copyright Brandon Fiechter.
   */
  val musicIn = AudioSystem.getAudioInputStream(new File("sound/music/gameplay1.wav"))
  val musicSound = AudioSystem.getClip
  musicSound.open(musicIn)
  
  val fightIn = AudioSystem.getAudioInputStream(new File("sound/effects/sword_fight.wav"))
  val fightSound = AudioSystem.getClip
  fightSound.open(fightIn)
  var fightSoundPlaying = false
  
  val beep = new File("sound/extra/beep.wav")
  
  def playSound(sfx: File) = {
    val audioIn = AudioSystem.getAudioInputStream(sfx)
    val clip = AudioSystem.getClip
    clip.open(audioIn)
    clip.start
  }
  
  def startFightSound() = {
    fightSound.loop(-1)
    fightSoundPlaying = true
  }
  
  def stopFightSound() = {
    fightSound.stop()
    fightSoundPlaying = false
  }
  
  def startGameMusic() = {
    musicSound.loop(-1)
  }
  
  def stopGameMusic() = {
    musicSound.stop()
  }
  
}