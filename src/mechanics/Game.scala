package mechanics

/*
 * The Game class requires an announcementFeed object in the GUI,
 * which has the announce() methods. Consequently, the mechanics
 * of the game must be allowed this one active connection to
 * communicate with the GUI.
 * Therefore, this feed is requested as a parameter and the feed
 * with the java.awt.Color that it requires are imported for the
 * sake of convenience:
 */

import java.awt.Color

class Game(val gameMap: Map, announcementFeed: mechanics.ui.AnnouncementFeed) {
  import announcementFeed.announce    //For convenience
  io.Loader.loadCharacters()
  io.Loader.loadBuildings()
  val rand = new scala.util.Random()
  var hasStarted = false
  var isOver = false
  var human: Human = null
  var ai: AI = null
  val framesPerTurn = 10    //A constant that decides the speed of the economy.
  
  private var totalFrameCount = 0
  
  def frameCount = this.totalFrameCount
  
  def setHuman(human: Human) = {
    this.human = human
    gameMap.humanTerritory.owner = human
  }
  
  def setAi(ai: AI) = {
    this.ai = ai
    gameMap.aiTerritory.owner = ai
  }
  
  def announceCharacterDeath(character: Character) = {
    val name = Data.specsOfCharacter(character.id)._2
    if (character.owner == human) {
      announce("A " + name + " was killed!", Color.RED)
    } else {
      announce("An enemy " + name + " was killed!", new Color(0x009900))
    }
  }
  
  def announceNoPath(player: Player) = {
    if (player == human) {
      announce("Can't find a path there!", Color.RED)
    }
  }
  
  def announceAny(msg: String, news: String) = {
    val colour = news match {
      case "good" =>  new Color(0x009900)
      case "bad" => Color.RED
      case "neutral2" => Color.BLUE
      case _ => Color.BLACK
    }
    announce(msg, colour)
  }
  
  def frame() = {
    if (!human.isAlive) {
      announceAny("Game over!", "bad")
      isOver = true
    } else if (!ai.isAlive) {
      announceAny("Congratulations, you were victorious!", "good")
      isOver = true
    }
    if (hasStarted) {
      frameCount match {
        case 2 => announce("Welcome to Age of Lords!", new Color(0x009900), 20)
        case 3 => announce("Your task is to prepare for war, defend yourself and eventually slay the enemy lord.", Color.RED)
        case _ => {}
      }
        
      if (frameCount % framesPerTurn == 0) {
        human.goldBeforeLastTurn = human.goldAfterLastTurn
        human.goldAfterLastTurn = human.gold
        human.addResource("gold", 10)
        ai.goldBeforeLastTurn = ai.goldAfterLastTurn
        ai.goldAfterLastTurn = ai.gold
        ai.addResource("gold", 10)
        for (province <- gameMap.provinces) {
          if (province.owner.isDefined) {
            val resource = rand.shuffle(Vector(("gold", 5), ("wood", 2), ("stone", 1))).head
            province.owner.get.addResource(resource._1, resource._2)
          }
        }
      }
      gameMap.frame()
      ai.act()
      totalFrameCount += 1
    }
    
    else {                //At the first frame, the following is to be executed.
      if (human == null) {
        throw new GameException("Human lacking.")
      } else if (ai == null) {
        throw new GameException("AI lacking.")
      } else {
        hasStarted = true
      }
    }

    
  }
}


/**
 * Describes a general error in the game mechanics.
 */
class GameException(msg: String) extends Exception(msg)
