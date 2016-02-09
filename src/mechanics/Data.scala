package mechanics

/**
 * The Data object contains modular values of different game components.
 * It also contains some convenience methods for accessing its data
 * from other game classes.
 * These values may be modified (some here, some in external text files)
 * for a modified game experience.
 */
object Data {
  
  val rand = new scala.util.Random()
  
  /**
   * The initial amount of gold a player starts with.
   * The hard-coded value is the default, but this can be customized.
   */
  val initialResources = (500, 40, 20)
  
  /**
   * The maximum time of a province. When thes time reaches 0, the province
   * is conquered.
   */
  val provinceCooldown = 999
  
  //id -> (1:speed, 2:name, 3:recruitment, 4:cost, 5: reach, 6:hp, 7:attack, 8:reloadTime)
  val characterSpecs = scala.collection.mutable.Map[Int, (Int, String, String, Int, Int, Int, Int, Int)](
      //0 -> (    30, "lord",       "nowhere",    0,   110, 1000,35, 1),
      //1 -> (    40, "spearman",   "barracks",   50,  120, 100, 5,  1),
      //2 -> (    40, "archer",     "barracks",   100, 400, 40,  10, 4),
      //5 -> (    20, "catapult",   "barracks",   500, 500, 300, 40, 10),
      //1234 -> ( 40, "derpman",    "barracks",   150, 100, 200, 10, 1)
      )
  
  /**
   * Allows adding new character types to the game.
   * The changes must, however, be paired with an update of the textures.
   */
  def addCharacter(characterID: Int, specs: (Int, String, String, Int, Int, Int, Int, Int)) = {
    characterSpecs += characterID -> specs
  }
  
  /**
   * The property values of the given character, as a tuple
   * (speed, name, recruitment, cost, reach, hp, attack, reloadTime)
   */
  def specsOfCharacter(charactedID: Int) = characterSpecs(charactedID)
  
  def characters = characterSpecs.keys.toVector
  
  /**
   * Calculates the actual damage inflicted by one character on another.
   * This is calculated as a random number between 0 and 2*dmg, floored to
   * the previous interger, where dmg is the average damage.
   */
  def calculateDamage(dmg: Int) = (dmg * rand.nextFloat * 2).toInt
  
  /**
   * The data below is the DEFAULT data for the buildings.
   * The building values can be changed in the external files.
   * id -> (1:name, 2:coinsCost, 3:woodCost, 4:stoneCost, 5: maxHp)
   */
  private val buildingSpecs = scala.collection.mutable.Map[Int, (String, Int, Int, Int, Int)](
      1 -> (    "watchtower",           60,    24,  0,   2000),
      2 -> (    "barracks",             300,   40,  10,  2000),
      3 -> (    "lumberjack",           100,   0,   0,   500),
      4 -> (    "quarry",               200,   50,  0,   1500),
      5 -> (    "wooden wall",          40,    16,  0,   2000),
      6 -> (    "wooden wall corner",   20,    8,   0,   1000),
      7 -> (    "stables",              500,   80,  30,  2500),
      8 -> (    "house",                100,   60,  40,  1000),
      9 -> (    "siege workshop",       400,   50,  30,  2500),
      10 ->(    "stonewall",            80,    8,   30,  8000),
      11 ->(    "stonewall corner",     40,    4,   15,  4000)
      )
  
  /**
   * The property values of the given character, as a tuple
   * (name, coinsCost, woodCost, stoneCost, maxHp)
   */
  def specsOfBuilding(buildingID: Int) = buildingSpecs(buildingID)
  
  /**
   * Allows modifying the building specs.
   * The changes must, however, be paired with an update of the textures.
   * BUILDINGS CANNOT BE ADDED OR REMOVED EXTERNALLY, as opposed to characters.
   */
  def addBuilding(buildingID: Int, specs: (String, Int, Int, Int, Int)) = {
    buildingSpecs += buildingID -> specs
  }
  
  /**
   * All existing buildings.
   */
  def buildings = buildingSpecs.keys.toVector.sorted
  
}