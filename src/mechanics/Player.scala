package mechanics

import scala.collection.mutable.Buffer

abstract class Player(val name: String, val game: Game) {
  private var alive = true
  private val characterBuffer = Buffer[Character]()
  private val buildingBuffer = Buffer[Building]()
  private val itemBuffer = Buffer[Item]()
  private val resources = scala.collection.mutable.Map(
      "gold" -> Data.initialResources._1,
      "wood" -> Data.initialResources._2,
      "stone" -> Data.initialResources._3)
  
  var goldBeforeLastTurn = 0
  var goldAfterLastTurn = 0
  
  /**
   * Convenience method that returns the amount of gold the player currently has.
   */
  def gold = this.resource("gold")
  
  /**
   * Calculates how much the player is currently earning per turn (X amount of frames, as defined in the Game class).
   */
  def goldPerTurn = goldAfterLastTurn - goldBeforeLastTurn
  
  /**
   * Calculates whether or not this player can afford to purchase this building,
   * in regard to all resource types.
   */
  def canAffordBuilding(buildingId: Int): Boolean = {
    this.resource("gold") >= Data.specsOfBuilding(buildingId)._2 &&
    this.resource("wood") >= Data.specsOfBuilding(buildingId)._3 &&
    this.resource("stone") >= Data.specsOfBuilding(buildingId)._4
  }
  
  /**
   * Calculates whether or not this player can afford to recruit this
   * unit, in regard to all resource types.
   */
  def canAffordUnit(characterId: Int): Boolean = {
    this.resource("gold") >= Data.specsOfCharacter(characterId)._4
  }
      
  /**
   * Reveals whether or not the player is currently alive.
   */
  def isAlive = this.alive
  
  /**
   * Kills the player.
   */
  def kill() = {
    this.alive = false
  }
  
  /**
   * Returns the spawn location (Position object) of new barrack troops, wrapped in an
   * Option, if the player currently owns a barracks building - None, otherwise.
   */
  def barrackSpawnLocation = {
    val barracks = this.buildingBuffer.filter(_.id == 2)
    if (!barracks.isEmpty) {
      Some(barracks(0).asInstanceOf[Barracks].spawnLocation)
    } else None
  }
  
  /**
   * Returns the spawn location (Position object) of new stable troops, wrapped in an
   * Option, if the player currently owns stables - None, otherwise.
   */
  def stableSpawnLocation = {
    val stables = this.buildingBuffer.filter(_.id == 7)
    if (!stables.isEmpty) {
      Some(stables(0).asInstanceOf[Stables].spawnLocation)
    } else None
  }
  
  /**
   * Returns the spawn location (Position object) of new siege workshop troops, wrapped in an
   * Option, if the player currently owns a siege workshop - None, otherwise.
   */
  def siegeSpawnLocation = {
    val siegeWorkshop = this.buildingBuffer.filter(_.id == 9)
    if (!siegeWorkshop.isEmpty) {
      Some(siegeWorkshop(0).asInstanceOf[SiegeWorkshop].spawnLocation)
    } else None
  }
  
  /**
   * Finds out and returns how much resources of the given resource type
   * the player currently has.
   * NB! This function REQUIRES that the given resourceType exists!
   * @params resourceType The name (lowercase) of the requested resource type.
   */
  def resource(resourceType: String) = resources(resourceType)
  
  /**
   * Adds resources of given type.
   * @params resourceType The name (lowercase) of the requested resource type.
   * @params change The amount of resources to be added (can be negative).
   */
  def addResource(resourceType: String, change: Int) = {
    this.resources(resourceType) += change
  }
  
  /**
   * Removes resources of a given type.
   * @params resourceType The name (lowercase) of the requested resource type.
   * @params change The amount of resources to be removed (can be negative).
   */
  def removeResource(resourceType: String, change: Int) = {
    this.resources(resourceType) -= change
  }
  
  /**
   * Sets resources of a given type.
   * @params resourceType The name (lowercase) of the requested resource type.
   * @params newValue The new amount of resources of the given type.
   */
  def setResource(resourceType: String, newValue: Int) = {
    this.resources(resourceType) = newValue
  }
  
  /**
   * Returns the complete list of this player's characters.
   */
  def characters = this.characterBuffer.toVector
  
  /**
   * Adds a character to the player's control.
   * @params character The character to be added.
   */
  def addCharacter(character: Character) = {
    this.characterBuffer += character
  }
  
  /**
   * Removes a character from the player's control.
   * @params character The character to be removed.
   */
  def removeCharacter(character: Character) = {
    this.characterBuffer -= character
  }
  
  /**
   * Returns the complete list of the player's buildings.
   */
  def buildings = this.buildingBuffer.toVector
  
  /**
   * Adds a building to the player's control.
   * @params building The building to be added.
   */
  def addBuilding(building: Building) = {
    this.buildingBuffer += building
  }
  
  /**
   * Removes a building from the player's control.
   * @params building The building to be removed.
   */
  def removeBuilding(building: Building) = {
    this.buildingBuffer -= building
  }
  
  /**
   * Returns a complete list of this player's items.
   */
  def items = this.itemBuffer.toVector
  
  /**
   * Adds an item to the player's control.
   * @params item The item to be added.
   */
  def addItem(item: Item) = {
    this.itemBuffer += item
  }
  
  /**
   * Removes an item from the player's control.
   * @params item The item to be removed.
   */
  def removeItem(item: Item) = {
    this.itemBuffer -= item
  }
  
}