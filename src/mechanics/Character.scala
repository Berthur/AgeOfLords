package mechanics

abstract class Character(initialLocation: Position, val owner: Player, val id: Int) {
  private var map = owner.game.gameMap
  map.addCharacter(this)
  owner.addCharacter(this)
  
  private var speed = Data.specsOfCharacter(id)._1
  var location = initialLocation
  var velocity = Velocity()
  var target: Option[Position] = None
  var nodes = collection.mutable.Stack[(Int, Int)]()
  var nextNode = if (!nodes.isEmpty) Some(nodes.pop) else None
  var hp = Data.specsOfCharacter(id)._6
  var inBattle = false
  var building: Option[Building] = None
  val reloadTime = Data.specsOfCharacter(id)._8
  var reloadCount = 0
  
  /**
   * Makes the necessary state changes when the character has used an attack.
   * Typically simulates having to reload a ranged weapon.
   */
  def attack() = {
    if (reloadTime != 1) {
      reloadCount = reloadTime
    }
  }
  
  /**
   * Tells whether the unit is ready to damage an enemy. This is always true for
   * common soldiers, while for example missile units and some siege equipment will
   * need a reload time.
   */
  def isReadyToAttack = if (reloadTime == 1) true else {reloadCount == 0}
  
  /**
   * Finds the next node in this character's path, if it exists,
   * and puts it as the next target node (nextNode).
   */
  def updateNextNode() = {
    nextNode = if (!nodes.isEmpty) Some(nodes.pop) else None
    if (!nextNode.isDefined) {
      this.stop()
    }
  }
  
  /**
   * Updates the velocity vector to point to the next target node (nextNode) if
   * this exists. Otherwise, stops the character.
   */
  def updateVelocity() = {
    if (nextNode.isDefined) {
      velocity = this.location.velocityTowards(nextNodePosition.get, this.speed)
    } else {
      this.stop()
    }
  }
  
  /**
   * Translates the nextNode grid coordinates into Map coordinates (Position).
   */
  def nextNodePosition: Option[Position] = {
    if (nextNode.isDefined) {
      Some(new Position(nextNode.get._1 * map.pointsPerCell + map.pointsPerCell / 2,
          nextNode.get._2 * map.pointsPerCell + map.pointsPerCell / 2))
    }
    else None
  }
  
  /**
   * Sets a new target position for the character, and tries to find a path to it.
   */
  def setTarget(targetLocation: Position) = {
    var realTarget = targetLocation
    if (map.containsPos(targetLocation)) {
      realTarget = targetLocation
    } else {
      realTarget = map.borderPosition(targetLocation)
    }
    target = Some(realTarget)
    val charOption = if (this.owner == this.owner.game.human) None else Some(this)
    nodes = map.gridCurrently.path((this.location.x / map.pointsPerCell, this.location.y / map.pointsPerCell),
        (target.get.x / map.pointsPerCell, target.get.y / map.pointsPerCell), this.owner, charOption)
    this.updateNextNode()
    this.updateVelocity()
  }
  
  /**
   * Stops the character completely.
   */
  def stop() = {
    this.target = None
    this.nodes = collection.mutable.Stack[(Int, Int)]()
    this.nextNode = None
    this.velocity = Velocity()
  }
  
  /**
   * Damages the character.
   */
  def damage(dmg: Int) = {
    this.hp -= Data.calculateDamage(dmg)
    if (this.hp <= 0) {
      this.kill()
    }
  }
  
  /**
   * Kills the character and removes it from the game.
   */
  def kill() = {
    owner.removeCharacter(this)
    map.removeCharacter(this)
    owner.game.announceCharacterDeath(this)
    if (building.isDefined) {
      building.get.asInstanceOf[Hollow].removeCharacters(this)
    }
    if (this.id == 0) {  //By definition, the death of a character with id=0 means defeat.
      this.owner.kill()
    }
  }
  
  /**
   * Simulates a frame: Calculates this character's new position, new velocity or
   * fights if in combat.
   */
  def frame() = {
    if (this.reloadCount != 0) {this.reloadCount -= 1}    //Reloads, if necessary.
    if (!inBattle) {    //A character does not move if he is in battle.
      if (this.owner.game.frameCount % 5 == 0) {this.hp += 1}  //Heals automatically, though slowly.
      if (target.isDefined && nextNode.isDefined && location.distanceTo(nextNodePosition.get) <= speed) {
        if (map.gridCurrently.fitsCharacter(nextNode.get._1, nextNode.get._2, this.owner)) {
          this.location = nextNodePosition.get
          this.updateNextNode()
          this.updateVelocity
        } else {                  //The situation has changed - calculate new path.
          val oldTarget = this.target
          this.stop()
          this.setTarget(oldTarget.get)
        }
      } else if (target.isDefined && nextNode.isDefined) {
        val newLocation = location.locationAfter(velocity, 1)
        if (map.gridCurrently.fitsCharacter(newLocation.x / map.pointsPerCell, newLocation.y / map.pointsPerCell, this.owner)) {
          if (map.containsPos(newLocation)) {
            location = location.locationAfter(velocity, 1)
          } else {
            location = map.borderPosition(newLocation)  //Moves this character to the border, just where it would have crossed it.
            this.stop()                                 //Resets this character's velocity to 0.
          }
        } else {                  //The situation has changed - calculate new path.
          val oldTarget = this.target
          this.stop()
          this.setTarget(oldTarget.get)
        }
      }
    }
  }
}