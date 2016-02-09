package mechanics

import scala.collection.mutable.Buffer

class Map(val width: Int, val height: Int, val pointsPerCell: Int, val humanTerritory: Territory, val aiTerritory: Territory, val provinces: Vector[Territory]) {
  
  
  private val charList = Buffer[Character]()
  
  private val itemList = Buffer[Item]()
  
  private val buildingList = Buffer[Building]()
  
  var selectedBuilding: Option[Building] = None      //This is an experiment
  
  private val terrainTable = Array.ofDim[Terrain.Value](width / pointsPerCell, height / pointsPerCell)
  
  var gridCurrently: Grid = this.createGrid
  
  def terrain = this.terrainTable.map(_.toVector).toVector
  
  def setTerrain(gridX: Int, gridY: Int, terrainType: Terrain.Value) = {
    this.terrainTable(gridX)(gridY) = terrainType
  }
  
  def allTerritories = this.humanTerritory +: this.aiTerritory +: provinces
  
  def containsPos(pos: Position) = 0 <= pos.x && pos.x < this.width && 0 <= pos.y && pos.y < this.height
  
  def borderPosition(pos: Position) = {
    val x = if (pos.x < 0) 0 else if (pos.x >= this.width) this.width - 1 else pos.x
    val y = if (pos.y < 0) 0 else if (pos.y >= this.height) this.height - 1 else pos.y
    new Position(x, y)
  }
  
  def posToGridCoords(pos: Position): (Int, Int) = {
    val gridX = pos.x / this.pointsPerCell
    val gridY = pos.y / this.pointsPerCell
    (gridX, gridY)
  }
  
  def gridCoordsToPosUpperLeft(gridX: Int, gridY: Int) = {
    val x = gridX * this.pointsPerCell
    val y = gridY * this.pointsPerCell
    new Position(x, y)
  }
  
  def gridCoordsToPosCentered(gridX: Int, gridY: Int) = {
    val upperLeft = this.gridCoordsToPosUpperLeft(gridX, gridY)
    new Position(upperLeft.x + this.gridWidth / 2, upperLeft.y + this.gridHeight / 2)
  }
  
  def gridWidth = this.width / this.pointsPerCell
  
  def gridHeight = this.height / this.pointsPerCell
  
  def characters = this.charList.toVector
  
  def items = this.itemList.toVector
  
  def buildings = this.buildingList.toVector
  
  def addCharacter(character: Character) = {
    this.charList += character
  }
  
  def removeCharacter(character: Character) = {
    this.charList -= character
  }
  
  def addItem(item: Item) = {
    this.itemList += item
  }
  
  def removeItem(item: Item) = {
    this.itemList -= item
  }
  
  private def addBuilding(building: Building) = {
    this.buildingList += building
  }
  
  private def removeBuilding(building: Building) = {
    this.buildingList -= building
  }
  
  /**
   * Attempts to place a building on the map. If successfully, places the building
   * and returns true. Else, returns false.
   */
  def build(building: Building, location: Position): Boolean = {
    val g = this.createGrid
    if (g.fitsBuilding(location.x / pointsPerCell, location.y / pointsPerCell, building)) {
      building.construct(location)
      this.addBuilding(building)
      building.owner.addBuilding(building)
      building.owner.removeResource("gold", Data.specsOfBuilding(building.id)._2)    //Discount the cost in coins
      building.owner.removeResource("wood", Data.specsOfBuilding(building.id)._3)    //Discount the cost in wood
      building.owner.removeResource("stone", Data.specsOfBuilding(building.id)._4)   //Discount the cost in stone
      true
    } else {
      false
    }
  }
  
  def destroy(building: Building) = {
    building.owner.removeBuilding(building)
    this.removeBuilding(building)
  }
  
  def frame() = {
    this.buildingList.foreach(_.frame())
    this.charList.foreach(character => if (character != null) character.frame())  //Some may have been killed before it's their turn.
    for (province <- this.provinces) {
      val flag = province.flagCoords.get    //Provinces always have a flag
      var intruders = collection.mutable.Set[Player]()
      var ownerTroops = 0
      var otherTroops = 0
      for (dx <- -1 to 1; dy <- -1 to 1) {
        val troops = gridCurrently.charactersAt(flag._1 + dx, flag._2 + dy)
        ownerTroops += (if (province.owner.isDefined) troops.count(unit => Some(unit.owner) == province.owner) else 0)
        otherTroops += troops.size - ownerTroops
        if (otherTroops != 0) {
          troops.filter(unit => Some(unit.owner) != province.owner).foreach(intruders += _.owner)
        }
      }
      if (intruders.size > 1) {}                      //Do nothing (fight)
      else if (ownerTroops != 0 && otherTroops != 0) {}    //Do nothing (fight)
      else if (ownerTroops != 0 && province.time != Data.provinceCooldown) {
        province.time = Math.min(province.time + ownerTroops, Data.provinceCooldown)  //Count up to max
      } else if (otherTroops != 0){                   //Count down to 0
        province.time -= otherTroops
        if (province.time <= 0) {
          province.owner = intruders.head
          province.time = Data.provinceCooldown
          for (cell <- province.cellCoords) {    //All buildings overlapping the province are destroyed upon conquest.
            val buildingOption = this.gridCurrently.buildingAt(cell._1, cell._2)
            if (buildingOption.isDefined && this.buildings.contains(buildingOption.get)) {
              buildingOption.get.destroy(false)
            }
          }
        }
      }
    }
  }
  
  def createGrid = {
    gridCurrently = new Grid(this)
    gridCurrently
  }
  
}