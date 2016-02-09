package mechanics.ui

import mechanics._

/**
 * This object contains data about the textures used for each building, character,
 * item etc. such as image file paths. Its purpose is to make the GUI more modular
 * and allow easy modifications.
 */
object Textures {
  
  /**
   * Takes a building and a building part (building coordinate tuple) as parameter
   * and returns building part's corresponding image file name and path.
   */
  def buildingPart(building: Building, part: (Int, Int)): String = {
    var filename = "textures/buildings/"
    
    building.id match {
      case 1 => {      //Watchtower
        part match {    //Indifferent orientation
          case (0,0) => filename += "watchtower_nw.png"
          case (1,0) => filename += "watchtower_ne.png"
          case (0,1) => filename += "watchtower_sw.png"
          case (1,1) => filename += "watchtower_se.png"
        }
      }
      case 2 => {    //Barracks
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "barracks_(0,0).png"
            case (1,0) => filename += "barracks_(1,0).png"
            case (2,0) => filename += "barracks_(2,0).png"
            case (0,1) => filename += "barracks_(0,1).png"
            case (1,1) => filename += "barracks_(1,1).png"
            case (2,1) => filename += "barracks_(2,1).png"
            case (0,2) => filename += "barracks_(0,2).png"
            case (2,2) => filename += "barracks_(2,2).png"
          }
        } else {
          part match {
            case (0,0) => filename += "barracks_v_(0,0).png"
            case (0,1) => filename += "barracks_v_(0,1).png"
            case (0,2) => filename += "barracks_v_(0,2).png"
            case (1,0) => filename += "barracks_v_(1,0).png"
            case (1,1) => filename += "barracks_v_(1,1).png"
            case (1,2) => filename += "barracks_v_(1,2).png"
            case (2,0) => filename += "barracks_v_(2,0).png"
            case (2,2) => filename += "barracks_v_(2,2).png"
          }
        }
      }
      case 3 => {    //Lumberjack
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "lumberjack_(0,0).png"
            case (0,1) => filename += "lumberjack_(0,1).png"
            case (0,2) => filename += "lumberjack_(0,2).png"
            case (1,0) => filename += "lumberjack_(1,0).png"
            case (1,1) => filename += "lumberjack_(1,1).png"
            case (1,2) => filename += "lumberjack_(1,2).png"
            case (2,0) => filename += "lumberjack_(2,0).png"
            case (2,1) => filename += "lumberjack_(2,1).png"
            case (2,2) => filename += "lumberjack_(2,2).png"
            case (3,1) => filename += "lumberjack_(3,1).png"
            case (3,2) => filename += "lumberjack_(3,2).png"
          }
        } else {
          part match {
            case (0,0) => filename += "lumberjack_v_(0,0).png"
            case (0,1) => filename += "lumberjack_v_(0,1).png"
            case (0,2) => filename += "lumberjack_v_(0,2).png"
            case (1,0) => filename += "lumberjack_v_(1,0).png"
            case (1,1) => filename += "lumberjack_v_(1,1).png"
            case (1,2) => filename += "lumberjack_v_(1,2).png"
            case (1,3) => filename += "lumberjack_v_(1,3).png"
            case (2,0) => filename += "lumberjack_v_(2,0).png"
            case (2,1) => filename += "lumberjack_v_(2,1).png"
            case (2,2) => filename += "lumberjack_v_(2,2).png"
            case (2,3) => filename += "lumberjack_v_(2,3).png"
          }
        }
      }
      case 4 => {    //Quarry
        part match {  //Indifferent orientation
          case (0,0) => filename += "quarry_(0,0).png"
          case (1,0) => filename += "quarry_(1,0).png"
          case (2,0) => filename += "quarry_(2,0).png"
          case (0,1) => filename += "quarry_(0,1).png"
          case (1,1) => filename += "quarry_(1,1).png"
          case (2,1) => filename += "quarry_(2,1).png"
          case (0,2) => filename += "quarry_(0,2).png"
          case (1,2) => filename += "quarry_(1,2).png"
          case (2,2) => filename += "quarry_(2,2).png"
        }
      }
      case 5 => {    //Wooden wall
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "woodenwall.png"
            case (1,0) => filename += "woodenwall.png"
          }
        } else {
          part match {
            case (0,0) => filename += "woodenwall_v.png"
            case (0,1) => filename += "woodenwall_v.png"
          }
        }
      }
      case 6 => {    //Wooden wall, corner
        part match {  //Indifferent orientation
          case (0,0) => filename += "woodenwall_corner.png"
        }
      }
      case 7 => {    //Stables
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "stables_(0,0).png"
            case (0,1) => filename += "stables_(0,1).png"
            case (0,2) => filename += "stables_(0,2).png"
            case (1,0) => filename += "stables_(1,0).png"
            case (1,1) => filename += "stables_(1,1).png"
            case (2,0) => filename += "stables_(2,0).png"
            case (2,1) => filename += "stables_(2,1).png"
            case (2,2) => filename += "stables_(2,2).png"
          }
        } else {
          part match {
            case (0,0) => filename += "stables_v_(0,0).png"
            case (0,1) => filename += "stables_v_(0,1).png"
            case (0,2) => filename += "stables_v_(0,2).png"
            case (1,0) => filename += "stables_v_(1,0).png"
            case (1,1) => filename += "stables_v_(1,1).png"
            case (1,2) => filename += "stables_v_(1,2).png"
            case (2,0) => filename += "stables_v_(2,0).png"
            case (2,2) => filename += "stables_v_(2,2).png"
          }
        }
      }
      case 8 => {    //House
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "corner1_nw.png"
            case (1,0) => filename += "corner1_ne.png"
            case (0,1) => filename += "side1_w.png"
            case (1,1) => filename += "tonguebase1_e.png"
            case (2,1) => filename += "tongue1_e.png"
            case (0,2) => filename += "corner1_sw.png"
            case (1,2) => filename += "corner1_se.png"
          }
        } else {
          part match {
            case (0,0) => filename += "corner1_nw.png"
            case (1,0) => filename += "side1_n.png"
            case (2,0) => filename += "corner1_ne.png"
            case (0,1) => filename += "corner1_sw.png"
            case (1,1) => filename += "tonguebase1_s.png"
            case (2,1) => filename += "corner1_se.png"
            case (1,2) => filename += "tongue1_s.png"
          }
        }
      }
      case 9 => {    //Siege workshop
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "siegeworkshop_(0,0).png"
            case (1,0) => filename += "siegeworkshop_(1,0).png"
            case (2,0) => filename += "siegeworkshop_(2,0).png"
            case (0,1) => filename += "siegeworkshop_(0,1).png"
            case (1,1) => filename += "siegeworkshop_(1,1).png"
            case (2,1) => filename += "siegeworkshop_(2,1).png"
            case (0,2) => filename += "siegeworkshop_(0,2).png"
            case (2,2) => filename += "siegeworkshop_(2,2).png"
          }
        } else {
          part match {
            case (0,0) => filename += "siegeworkshop_v_(0,0).png"
            case (0,1) => filename += "siegeworkshop_v_(0,1).png"
            case (0,2) => filename += "siegeworkshop_v_(0,2).png"
            case (1,0) => filename += "siegeworkshop_v_(1,0).png"
            case (1,1) => filename += "siegeworkshop_v_(1,1).png"
            case (1,2) => filename += "siegeworkshop_v_(1,2).png"
            case (2,0) => filename += "siegeworkshop_v_(2,0).png"
            case (2,2) => filename += "siegeworkshop_v_(2,2).png"
          }
        }
      }
      case 10 => {    //Stonewall
        if (building.horizontal) {
          part match {
            case (0,0) => filename += "stonewall.png"
            case (1,0) => filename += "stonewall.png"
          }
        } else {
          part match {
            case (0,0) => filename += "stonewall_v.png"
            case (0,1) => filename += "stonewall_v.png"
          }
        }
      }
      case 11 => {    //Stonewall corner
        part match {  //Indifferent orientation
          case (0,0) => filename += "stonewall_corner.png"
        }
      }
      
      
    }

    
    filename
  }
  
  /**
   * id -> (texture, selected texture, enemy texture, character card textuer)
   */
  private val characterTextures = collection.mutable.Map[Int, (String, String, String, String)]()
  
  /**
   * Adds character texture of a character.
   * NB! MUST be matched with a character of the same ID in data/characters !
   */
  def addCharacter(id: Int, texture: String, selectedTexture: String, enemyTexture: String, cardTexture: String) = {
    characterTextures += id -> (texture, selectedTexture, enemyTexture, cardTexture)
  }
  
  /**
   * Takes a character ID and a 'selected'-boolean as parameters and
   * returns the character's corresponding texture, possibly taking
   * into account whether the character is selected at that moment.
   */
  def character(id: Int, selected: Boolean, friendly: Boolean): String = {
    var filename = "textures/characters/"
    (selected, friendly) match {
      case (true, true) => filename + this.characterTextures(id)._2
      case (false, true) => filename + this.characterTextures(id)._1
      case _ => filename + this.characterTextures(id)._3
    }
  }
  
  def buildingCard(id: Int) = {
    var filename = "textures/buildingcards/"
    id match {
      case 1 => filename += "watchtower.png"
      case 2 => filename += "barracks.png"
      case 3 => filename += "lumberjack.png"
      case 4 => filename += "quarry.png"
      case 5 => filename += "woodenwall.png"
      case 6 => filename += "woodenwall.png"
      case 7 => filename += "stables.png"
      case 8 => filename += "house.png"
      case 9 => filename += "siegeworkshop.png"
      case 10 => filename += "stonewall.png"
      case 11 => filename += "stonewall.png"
    }
    filename
  }
  
  def unitCard(id: Int) = {
    var filename = "textures/unitcards/"
    filename + this.characterTextures(id)._4
  }
  
}