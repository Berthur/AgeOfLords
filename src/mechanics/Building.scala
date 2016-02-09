package mechanics

abstract class Building(val owner: Player) {
  
  /**
   * Whether or not this building is in horizontal mode.
   */
  var horizontal = true
  
  /**
   * The maximum (and initial) amount of hp this building can have.
   */
  val maxHp = {
    Data.specsOfBuilding(this.id)._5
  }
  
  /**
   * The current health points this building has.
   * When the hp reaches 0, the building is destroyed.
   */
  var hp = maxHp
  
  /**
   * The Map Position of the building's upper left corner.
   */
  var location: Option[Position] = None
  
  /**
   * The ID of the building.
   */
  def id: Int
  
  /**
   * Tells whether this building is hollow; that is, if it can contain
   * characters.
   */
  def isHollow = false
  
  /**
   * Tells whether this building is considered a fortification and can
   * be built on rough terrain (does not necessarily need to correlate
   * with whether the building REALLY is a fortification).
   */
  def isFortification = false

  
  /**
   * Announces the construction of this building at the given location.
   * Normally, the player is then charged and sometimes (if method is
   * overridden in a Building subclass), other things may happen.
   * If this building is already placed, this method does nothing.
   */
  def construct(pos: Position) = {
    if (this.location == None) {
      this.location = Some(pos)
    } else {}
  }
  
  /**
   * Destroys the building.
   * @param purpose Whether or not this building was deleted on purpose or destroyed by an enemy.
   * If deleted, some resources may be refunded.
   */
  def destroy(purpose: Boolean) = {
    if (purpose) {
      val goldRefund = Data.specsOfBuilding(this.id)._2 / 4
      val woodRefund = Data.specsOfBuilding(this.id)._3 / 4
      val stoneRefund = Data.specsOfBuilding(this.id)._4 / 4
      this.owner.addResource("gold", goldRefund)
      this.owner.addResource("wood", woodRefund)
      this.owner.addResource("stone", stoneRefund)
      this.owner.game.announceAny(this.name.head.toUpper + this.name.tail + " was demolished!", "neutral2")
    } else {
      this.owner.game.announceAny(this.name.head.toUpper + this.name.tail + " was destroyed!", "bad")
    }
    this.owner.game.gameMap.destroy(this)
    if (this.isHollow) {
      this.asInstanceOf[Hollow].characters.foreach(_.building = None)
    }
  }
  
  /**
   * Damages the building.
   */
  def damage(dmg: Int) = {
    this.hp -= dmg
    if (this.hp <= 0) {
      this.destroy(false)
    }
  }
  
  /**
   * Returns the name of this building, as displayed in the game.
   */
  def name = Data.specsOfBuilding(id)._1
  
  /**
   * The building's cost in gold. NB: Buildings typically cost other
   * resources as well.
   */
  def cost = Data.specsOfBuilding(id)._2
  
  /**
   * The shape of this building in horizontal mode, expressed in grid
   * coordinates such that the upper left cell is at (0,0).
   */
  def horizontalShape: Vector[(Int, Int)]
  
  /**
   * The shape of this building, taking into account the possibility
   * of its being vertical.
   */
  def shape = {
    if (horizontal) horizontalShape
    else horizontalShape.map( tuple => (tuple._2, tuple._1) )
  }
  
  /**
   * The part of this building that would be used to display information
   * in the GUI, if required. Expressed in grid coordinates such that the
   * upper left cell is at (0,0).
   */
  def representativePart = (1,1)
  
  /**
   * The real grid coordinates of this building's representative part.
   */
  def representativeCoords: (Int, Int) = {
    if (horizontal) {
      val cellX = location.get.x / owner.game.gameMap.pointsPerCell + representativePart._1
      val cellY = location.get.y / owner.game.gameMap.pointsPerCell + representativePart._2
      (cellX, cellY)
    } else {
      val cellX = location.get.x / owner.game.gameMap.pointsPerCell + representativePart._2
      val cellY = location.get.y / owner.game.gameMap.pointsPerCell + representativePart._1
      (cellX,cellY)
    }
  }
  
  /**
   * Returns the GRID(!) coordinates of all cells that contain this building.
   */
  def coords = {
    val cellSize = this.owner.game.gameMap.pointsPerCell
    if (this.location.isDefined) {
      this.shape.map(tuple => (tuple._1 + this.location.get.x / cellSize , tuple._2 + this.location.get.y / cellSize))
    } else {
      Vector[(Int, Int)]()
    }
  }
  
  /**
   * Returns a vector of the  grid coordinates of this building, paired with its corresponding
   * relative building part (as given by this.shape).
   */
  def coordsWithRelativeCoords = this.coords zip this.shape
  
  /**
   * Performs the specific building's mutable actions at every frame. These actions
   * are typically generating resources for the player, taking its upkeep cost or similar.
   * Also, if the building is damaged, it typically heals slowly.
   */
  def frame() = {
    if (this.hp < this.maxHp) {
      this.hp = math.min(this.hp + 5, this.maxHp)
    }
  }
}

/**
 * This trait describes the property of a building to be hollow; that is, to be allowed
 * to contain characters, contrary to other buildings. A character can be ordered to
 * enter building only if it is hollow - and in this case it will autmatically be set to
 * walk to the building's defined center position, where all its characters will be
 * concentrated. In a typical GUI, clicking on the building will offer access to the
 * set of units it contains.
 */
trait Hollow extends Building {
  
  private val characterBuffer = collection.mutable.Buffer[Character]()
  
  /**
   * The characters that this hollow building contains.
   */
  def characters = characterBuffer.toVector
  
  /**
   * Adds characters to this hollow building.
   */
  def addCharacters(characters: Character*) = {
    characters.foreach(char => {
      if (char.owner == this.owner && char.id != 0) {  //Only characters of the same faction as the building may enter it, and never the lord.
        char.setTarget(this.containerLocation)    //Concentrates all the characters in this center position.
        if (char.target.isDefined ||  //Checks if the characters has actually found a path to the building (or is already inside).
            char.location.distanceTo(this.containerLocation) < this.owner.game.gameMap.pointsPerCell) {
          characterBuffer += char
          char.building = Some(this)
        }
      }
    })
  }
  
  /**
   * Removes characters from this hollow building.
   */
  def removeCharacters(character: Character*) = {
    character.foreach(char => {
      characterBuffer -= char
      char.building = None
    })
  }
  
  /**
   * A hollow building is, perhaps to one's great surprise, hollow.
   */
  override def isHollow = true
  
  /**
   * Hollow buildings can be constructed on top of own troops.
   * Thus, its construct method implements this feature.
   */
  override def construct(pos: Position) = {
    super.construct(pos)
    val grid = this.owner.game.gameMap.createGrid
    val cells = this.coords
    for (cell <- cells) {
      val characters = grid.charactersAt(cell._1, cell._2).filter(_.owner == this.owner)
      this.addCharacters(characters:_*)
    }
  }
  
  /**
   * The center of this building (the position where all the building's units are supposed
   * to be located), expressed as the distance, in grid coords, from the upper left corner
   * of the building.
   */
  def center: (Double, Double)
  
  /**
   * Calculates the Map Position where all the building's units are supposed to be located.
   */
  lazy val containerLocation = {
    require(location.isDefined)
    if (horizontal) {
      val posX = location.get.x + (center._1 * owner.game.gameMap.pointsPerCell).toInt
      val posY = location.get.y + (center._2 * owner.game.gameMap.pointsPerCell).toInt
      new Position(posX, posY)
    } else {
      val posX = location.get.x + (center._2 * owner.game.gameMap.pointsPerCell).toInt
      val posY = location.get.y + (center._1 * owner.game.gameMap.pointsPerCell).toInt
      new Position(posX, posY)
    }
  }
}

/**
 * The Building class' Company Object.
 */
object Building {
  
  /**
   * Identifies the building from its id (passed as an argument), matches it with its
   * particular class and creates it, in the 'owner''s name.
   */
  def createBuilding(id: Int, owner: Player, horizontal: Boolean) = {
    val building = 
      id match {
        case 1 => new Watchtower(owner)
        case 2 => new Barracks(owner)
        case 3 => new Lumberjack(owner)
        case 4 => new Quarry(owner)
        case 5 => new WoodenWall(owner)
        case 6 => new WoodenWallCorner(owner)
        case 7 => new Stables(owner)
        case 8 => new House(owner)
        case 9 => new SiegeWorkshop(owner)
        case 10 => new Stonewall(owner)
        case 11 => new StonewallCorner(owner)
        //case 1234 => new DerpBuilding(owner)
      }
    building.horizontal = horizontal
    building
  }
  
}


class Watchtower(owner: Player) extends Building(owner) with Hollow {
  private val horizontalShapeVector = Vector((0,0), (1,0), (0,1), (1,1))
  def horizontalShape = this.horizontalShapeVector
  def id = 1
  override def isFortification = true
  override val representativePart = (0,0)
  val center = (1.0, 1.0)    //The actual MIDDLE (unlike the spawn position system)
}

class Barracks(owner: Player) extends Building(owner) {
  private val horizontalShapeVector = Vector((0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (0,2), (2,2))
  def horizontalShape = this.horizontalShapeVector
  def id = 2
  var spawnLocation: Position = null
  private def setSpawnLocation = {
    if (horizontal) {
      val spawnX = location.get.x + (1.5 * owner.game.gameMap.pointsPerCell).toInt
      val spawnY = location.get.y + (2.5 * owner.game.gameMap.pointsPerCell).toInt
      spawnLocation = new Position(spawnX, spawnY)
    } else {
      val spawnX = location.get.x + (2.5 * owner.game.gameMap.pointsPerCell).toInt
      val spawnY = location.get.y + (1.5 * owner.game.gameMap.pointsPerCell).toInt
      spawnLocation = new Position(spawnX, spawnY)
    }
  }
  override def frame() = {
    super.frame()
    if (location.isDefined && spawnLocation == null) {setSpawnLocation}
  }
}

class Lumberjack(owner: Player) extends Building(owner) {
  private val horizontalShapeVector = Vector((0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (3,1), (0,2), (1,2), (2,2), (3,2))
  def horizontalShape = this.horizontalShapeVector
  def id = 3
  override def frame() = {
    super.frame()
    if (owner.game.frameCount % 15 == 0) {
      this.owner.addResource("wood", 1)
    }
  }
}

class Quarry(owner: Player) extends Building(owner) {
  private val horizontalShapeVector = Vector((0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (0,2), (1,2), (2,2))
  def horizontalShape = this.horizontalShapeVector
  def id = 4
  override def frame() = {
    super.frame()
    if (owner.game.frameCount % 20 == 0) {
      this.owner.addResource("stone", 1)
    }
  }
}

class WoodenWall(owner: Player) extends Building(owner) with Hollow {
  private val horizontalShapeVector = Vector((0,0), (1,0))
  def horizontalShape = this.horizontalShapeVector
  def id = 5
  override def isFortification = true
  override def representativePart = (0,0)
  def center = (1.0, 0.5)
}

class WoodenWallCorner(owner: Player) extends Building(owner) with Hollow {
  private val horizontalShapeVector = Vector((0,0))
  def horizontalShape = this.horizontalShapeVector
  def id = 6
  override def isFortification = true
  override def representativePart = (0,0)
  def center = (0.5, 0.5)
}

class Stables(owner: Player) extends Building(owner) {
  private val horizontalShapeVector = Vector((0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (0,2), (2,2))
  def horizontalShape = this.horizontalShapeVector
  def id = 7
  var spawnLocation: Position = null
  private def setSpawnLocation = {
    if (horizontal) {
      val spawnX = location.get.x + (1.5 * owner.game.gameMap.pointsPerCell).toInt
      val spawnY = location.get.y + (2.5 * owner.game.gameMap.pointsPerCell).toInt
      spawnLocation = new Position(spawnX, spawnY)
    } else {
      val spawnX = location.get.x + (2.5 * owner.game.gameMap.pointsPerCell).toInt
      val spawnY = location.get.y + (1.5 * owner.game.gameMap.pointsPerCell).toInt
      spawnLocation = new Position(spawnX, spawnY)
    }
  }
  override def frame() = {
    super.frame()
    if (location.isDefined && spawnLocation == null) {setSpawnLocation}
  }
}

class House(owner: Player) extends Building(owner) {
  private val horizontalShapeVector = Vector((0,0), (1,0), (0,1), (1,1), (2, 1), (0, 2), (1, 2))
  def horizontalShape = this.horizontalShapeVector
  def id = 8
  override def frame() = {
    super.frame()
    if (owner.game.frameCount % 5 == 0) {
      this.owner.addResource("gold", 1)
    }
  }
}

class SiegeWorkshop(owner: Player) extends Building(owner) {
  private val horizontalShapeVector = Vector((0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (0,2), (2,2))
  def horizontalShape = this.horizontalShapeVector
  def id = 9
  var spawnLocation: Position = null
  private def setSpawnLocation = {
    if (horizontal) {
      val spawnX = location.get.x + (1.5 * owner.game.gameMap.pointsPerCell).toInt
      val spawnY = location.get.y + (2.5 * owner.game.gameMap.pointsPerCell).toInt
      spawnLocation = new Position(spawnX, spawnY)
    } else {
      val spawnX = location.get.x + (2.5 * owner.game.gameMap.pointsPerCell).toInt
      val spawnY = location.get.y + (1.5 * owner.game.gameMap.pointsPerCell).toInt
      spawnLocation = new Position(spawnX, spawnY)
    }
  }
  override def frame() = {
    super.frame()
    if (location.isDefined && spawnLocation == null) {setSpawnLocation}
  }
}

class Stonewall(owner: Player) extends Building(owner) with Hollow {
  private val horizontalShapeVector = Vector((0,0), (1,0))
  def horizontalShape = this.horizontalShapeVector
  def id = 10
  override def isFortification = true
  override def representativePart = (0,0)
  def center = (1.0, 0.5)
}

class StonewallCorner(owner: Player) extends Building(owner) with Hollow {
  private val horizontalShapeVector = Vector((0,0))
  def horizontalShape = this.horizontalShapeVector
  def id = 11
  override def isFortification = true
  override def representativePart = (0,0)
  def center = (0.5, 0.5)
}
