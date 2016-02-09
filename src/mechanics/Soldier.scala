package mechanics

/**
 * This class represents a warrior character, which purpose is to
 * simply inflcit damage on any enemy characters in range (soldiers
 * or otherwise). Some soldiers can also damage buildings.
 * This class also holds the basics of the combat system.
 */
class Soldier(initialLocation: Position, owner: Player, id: Int) extends Character(initialLocation, owner, id) {
  
  override def frame() = {
    super.frame()
    val specs = Data.specsOfCharacter(this.id)
    val map = this.owner.game.gameMap
    if (this.isReadyToAttack) {
      val enemy = (if (this.owner == this.owner.game.human) this.owner.game.ai else this.owner.game.human)
      if (!enemy.characters.isEmpty) {
        val possibleTargets = enemy.characters.filter(!_.building.isDefined)  //Can't damage characters inside a building.
        if (!possibleTargets.isEmpty) {
          val target = possibleTargets.minBy[Double](_.location.distanceTo(this.location))  //Finds the closest enemy
          val distance = target.location.distanceTo(this.location)
          if (distance <= specs._5) {
            this.inBattle = true
            target.damage(specs._7)
            this.attack()
          } else if (this.velocity.isZero && this.owner != this.owner.game.human && distance <= this.owner.game.gameMap.pointsPerCell * 6) {
            this.setTarget(target.location)
          } else {
            this.inBattle = false
            val troopType = specs._2
            val buildingCandidates = enemy.buildings
            if (!buildingCandidates.isEmpty) {
              val buildingParts = buildingCandidates.map(_.coords).flatten
              val targetBuildingPart = buildingParts.minBy(coords => this.location.distanceTo(
                  new Position(coords._1 * map.pointsPerCell + map.pointsPerCell / 2, coords._2 * map.pointsPerCell + map.pointsPerCell / 2)))
              val distance = this.location.distanceTo(new Position(
                  targetBuildingPart._1 * map.pointsPerCell + map.pointsPerCell / 2, targetBuildingPart._2 * map.pointsPerCell + map.pointsPerCell / 2))
              val targetBuilding = map.gridCurrently.buildingAt(targetBuildingPart._1, targetBuildingPart._2)
              if (distance <= specs._5 && targetBuilding.isDefined) {
                targetBuilding.get.damage(specs._7)
              }
            }
          }
        }
      } else {
        this.inBattle = false
      }
    }
  }
}