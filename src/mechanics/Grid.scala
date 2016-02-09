package mechanics

import scala.collection.mutable.{Buffer, Stack}

class Grid(map: Map) {
  
  private val grid = Array.ofDim[Cell](map.gridWidth, map.gridHeight)
  //The following fills the grid with all the map contains:
  for (x <- 0 until map.gridWidth; y <- 0 until map.gridHeight) {
    this.grid(x)(y) = new Cell(map.terrain(x)(y))
  }
  for (c <- map.characters) {
    val coords = this.toGridCoords(c.location)
    grid(coords._1)(coords._2).add(c)
  }
  for (i <- map.items) {
    if (i.location.isDefined) {
      val coords = this.toGridCoords(i.location.get)
      grid(coords._1)(coords._2).add(i)
    }
  }
  for (b <- map.buildings; c <- b.coordsWithRelativeCoords) {
    val coords = c._1
    val part = c._2
    grid(coords._1)(coords._2).add(b, part)
  }
  
  /**
   * Returns a 2-dimensional array of booleans, one for each cell in the grid,
   * where each boolean informs whether or not that cell can be trespassed by
   * a character.
   */
  lazy val toNodes = grid.map( column => column.map( cell => {
    Terrain.fitsCharacter(cell.terrainType) &&                //Only penetrable terrain can be trespassed.
    (!cell.building.isDefined || cell.building.get.isHollow)  //Only hollow buildings can be trespassed.
    } ) )
  
  /**
   * Returns the width (in cells) of the grid.
   */
  def width = grid.size
  
  /**
   * Returns the height (in cells) of the grid.
   */
  def height = grid(0).size
  
  /**
   * Tells if the given grid coordinates exist.
   */
  def containsCoords(x: Int, y: Int) = 0 <= x && x < grid.size && 0 <= y && y < grid(0).size
  
  /**
   * Returns a list of the 8 immediate neighbours to the cell at (x, y), if the grid
   * contains these neighbours. If containsSelf is true, the result vector will also
   * include the input coordinates themselves.
   */
  def neighbours(x: Int, y: Int, containsSelf: Boolean): Vector[(Int, Int)] = {
    var result = Vector[(Int, Int)]()
    for (dx <- -1 to 1; dy <- -1 to 1) {
      val candidate = (x + dx, y + dy)
      if (this.containsCoords(candidate._1, candidate._2) && (containsSelf || candidate != (x,y))) {
        result = result :+ candidate
      }
    }
    result
  }
  
  /**
   * Returns the cell at grid coordinates x, y.
   */
  def cellAt(x: Int, y: Int) = grid(x)(y)
  
  /**
   * Returns the cell at map position pos.
   */
  def cellAt(pos: Position) = grid(toGridCoords(pos)._1)(toGridCoords(pos)._2)
  
  /**
   * Returns the terrain type at grid coordinates x, y.
   */
  def terrainAt(x: Int, y: Int) = grid(x)(y).terrainType
  
  /**
   * Returns the terrain type at map position pos.
   */
  def terrainAt(pos: Position) = grid(toGridCoords(pos)._1)(toGridCoords(pos)._2).terrainType
  
  /**
   * Returns a vector of all the characters at grid coordinates x, y.
   */
  def charactersAt(x: Int, y: Int) = grid(x)(y).characters
  
  /**
   * Returns a vector of all the characters at map position pos.
   */
  def charactersAt(pos: Position) = grid(toGridCoords(pos)._1)(toGridCoords(pos)._2).characters
  
  /**
   * Returns a vector of all the items at grid coordinates x, y.
   */
  def itemsAt(x: Int, y: Int) = grid(x)(y).items
  
  /**
   * Returns a vector of all the items at map position pos.
   */
  def itemsAt(pos: Position) = grid(toGridCoords(pos)._1)(toGridCoords(pos)._2).items
  
  /**
   * Returns an option, possibly containing the building at grid coordinates x, y.
   */
  def buildingAt(x: Int, y: Int) = grid(x)(y).building
  
  /**
   * Returns an option, possibly containing the building at map position pos.
   */
  def buildingAt(pos: Position) = grid(toGridCoords(pos)._1)(toGridCoords(pos)._2).building
  
  /**
   * Returns an option, possibly containing the building part coordinates at grid coordinates x, y.
   */
  def buildingPartAt(x: Int, y: Int) = grid(x)(y).buildingPart
  
  /**
   * Returns an option, possibly containing the building part coordinates at map position pos.
   */
  def buildingPartAt(pos: Position) = grid(toGridCoords(pos)._1)(toGridCoords(pos)._2).buildingPart
  
  def territoryAt(x: Int, y: Int): Option[Territory] = {
    for (territory <- this.map.allTerritories) {
      if (territory.cellCoords.contains((x, y))) {
        return Some(territory)
      }
    }
    return None
  }
  
  /**
   * Finds out if the given building would fit at the given grid coordinates.
   */
  def fitsBuilding(x: Int, y: Int, building: Building): Boolean = {
    val coords = building.shape.map( tuple => (tuple._1 + x, tuple._2 + y) )
    for (cell <- coords) {
      if (!containsCoords(cell._1, cell._2)) {
        return false
      }
      if (!Terrain.fitsBuilding(cellAt(cell._1, cell._2).terrainType, building.isFortification)) {
        return false
      }
      if (!building.isHollow && !cellAt(cell._1, cell._2).isEmpty) {
        return false
      }
      if (cellAt(cell._1, cell._2).characters.find(char => char.owner != building.owner || char.id == 0).isDefined) {
        return false
      }
      if (cellAt(cell._1, cell._2).building.isDefined) {
        return false
      }
      val territoryOption = territoryAt(cell._1, cell._2)
      if (!territoryOption.isDefined || !territoryOption.get.owner.isDefined || territoryOption.get.owner.get != building.owner) {
        return false
      }
    }
    true
  }
  
  /**
   * Finds out if a character can exist on the cell at (x, y).
   */
  def fitsCharacter(x: Int, y: Int, owner: Player): Boolean = {
    this.containsCoords(x, y) &&
    Terrain.fitsCharacter(this.terrainAt(x, y)) &&
    (!this.buildingAt(x, y).isDefined ||
        (this.buildingAt(x, y).get.isHollow) &&
        this.buildingAt(x, y).get.owner == owner)  //Hollow buildings can only be occupied by characters of the same faction.
  }
  
  /**
   * Calculates the grid coordinates of the map position pos.
   */
  private def toGridCoords(pos: Position): (Int, Int) = {
    val x = pos.x / map.pointsPerCell
    val y = pos.y / map.pointsPerCell
    (x, y)
  }
  
  /**
   * Finds a path from start to target, in the form of grid coordinates.
   * This method uses a variation of the A* pathfinding algorithm and should be able to
   * find a path from any point on the map to another, taking advantage of the fact that
   * the game maps, in their grid form, are never very large. The algorithm uses many
   * rough approximations and favours efficiency over mathematical perfectionism.
   * The method also attempts to make an announcement to the owner's Game object when no
   * path has been found.
   * This method uses the Node class for its algorithm.
   * If a character is passed in the character argument, and no path is found, the algorithm
   * will instead return the first building that is in the way, and that the character is
   * able to destroy. If no such building is found, returns an empty path.
   */
  def path(start: (Int, Int), target: (Int, Int), owner: Player, character: Option[Character]): Stack[(Int, Int)] = {
    var partialTarget: Option[(Int, Int)] = None
    if (!this.fitsCharacter(target._1, target._2, owner) || !this.containsCoords(start._1, start._2)) {
      return Stack[(Int, Int)]()
    }
    else {  //Start pathfinding:
      val startNode = new Node(start._1, start._2, None, start, target)
      val result = Stack[(Int, Int)]()
      val open = Buffer[Node](startNode)
      val closed = Buffer[Node]()
      var found = false
      while (!found) {
        if (!open.isEmpty) {
          val current = open.minBy(_.f)
          open -= current
          closed += current
          if ((current.x, current.y) == target) {  //Found target
            var i = current
            while ((i.x, i.y) != start) {
              result.push(i.toCoords)
              i = i.parent.get
            }
            return result
          }
          for (dx <- -1 to 1; dy <- -1 to 1) {  //For all 8 neighbours and the current node itself:
            val neighbour = (current.x + dx, current.y + dy)
            //Check that the neighbour is not on the closed list and it fits a character:
            if (!closed.find(node => (node.x, node.y) == neighbour).isDefined &&
                this.fitsCharacter(neighbour._1, neighbour._2, owner)) {
              val neighbourNode = open.find(node => (node.x, node.y) == neighbour)
              if (neighbourNode.isDefined) {
                val d = if (dx == 0 || dy == 0) Node.horizontalD else Node.diagonalD
                if (neighbourNode.get.g > current.g + d) {  //Found a shorter path to neighbourNode
                  neighbourNode.get.parent = Some(current)
                  neighbourNode.get.updateValues()
                }                                           //Else do nothing
              } else {
                open += new Node(neighbour._1, neighbour._2, Some(current), start, target)
              }
            } else {
              if (character.isDefined && !partialTarget.isDefined && this.containsCoords(neighbour._1, neighbour._2)) {
                val buildingOption = this.buildingAt(neighbour._1, neighbour._2)
                if (buildingOption.isDefined && buildingOption.get.owner != owner) {
                    partialTarget = Some(current.x, current.y)  //An accessible neighbour to the target building.
                }
              }
            }
          }
        } else {      //The open list is empty: no path was found.
          if (character.isDefined && partialTarget.isDefined) {
            return path(start, partialTarget.get, owner, character)
          } else {
            owner.game.announceNoPath(owner)
            return Stack[(Int, Int)]()
          }
        }
      }
      
      result          //Should not reach this point.
    }
  }
  
  /**
   * Finds a clean path (if no path is found all the way to the target, returns no path).
   */
  def path(start: (Int, Int), target: (Int, Int), owner: Player): Stack[(Int, Int)] = this.path(start, target, owner, None)
  
  
  
  override def toString = {
    var s = "=====GRID=====\n"
    for (y <- 0 until map.gridHeight) {
      for (x <- 0 until map.gridWidth) {
        if (this.buildingAt(x, y).isDefined) {
          s += "B "
        } else if (!this.charactersAt(x, y).isEmpty) {
          s += "C "
        } else if (!this.itemsAt(x, y).isEmpty) {
          s += "I "
        } else {
          s += this.terrainAt(x, y) + " "
        }
      }
      s += "\n"
    }
    s += "\n"
    s
  }
}