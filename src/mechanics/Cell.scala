package mechanics

import scala.collection.mutable.Buffer

class Cell(val terrainType: Terrain.Value) {
  private val charBuffer = Buffer[Character]()
  private var buildingOption: Option[Building] = None
  private var buildingPartOption: Option[(Int, Int)] = None
  private val itemBuffer = Buffer[Item]()
  
  def add(character: Character) = {
    charBuffer += character
  }
  def add(building: Building, part: (Int, Int))= {
    buildingOption = Some(building)
    buildingPartOption = Some(part)
  }
  def add(item: Item) = {
    itemBuffer += item
  }
  
  def characters = charBuffer.toVector
  
  def building = buildingOption
  
  def buildingPart = buildingPartOption
  
  def items = itemBuffer.toVector
  
  def isEmpty = characters.isEmpty && building.isEmpty && items.isEmpty
  
}

object Cell {
  def apply() = new Cell(Terrain.PLAIN)
}