package mechanics

import collection.mutable.Buffer

class AI(name: String, game: Game) extends Player(name, game) {
  
  val rand = new util.Random
  val map = game.gameMap
  val territory = map.aiTerritory
  val cellCoords = territory.cellCoords
  val originalGrid = map.createGrid
  def currentGrid = map.gridCurrently
  var lifeGuards = this.characters.filter(_.id != 0)
  var defensive = Vector[Character]()
  var offensive = Vector[Character]()
  var attacking = Vector[Character]()
  var provinceGuard = Vector[Character]()
  var centralPlaza = Vector[(Int, Int)]()
  var mainStreet = Vector[(Int, Int)]()
  lazy val barracksCoords = centralPlaza(8 * 1+ 1)
  lazy val siegeCoords = centralPlaza(8 * 4 + 1)
  lazy val stableCoords = centralPlaza(8 * 4 + 4)
  lazy val lordCoords = centralPlaza(8 * 2 + 5)
  lazy val offensiveCoords = centralPlaza(5)
  def frontierCoords = this.originalFrontier(this.rand.nextInt(this.originalFrontier.size))
  lazy val barracksPosition = this.positionAt(barracksCoords, false)
  lazy val siegePosition = this.positionAt(siegeCoords, false)
  lazy val stablePosition = this.positionAt(stableCoords, false)
  lazy val lordPosition = this.positionAt(lordCoords, true)
  lazy val offensivePosition = this.positionAt(offensiveCoords, true)
  private def guardCoords = this.currentGrid.neighbours(lordCoords._1, lordCoords._2, false)(rand.nextInt(8))
  private def guardPosition = this.positionAt(this.guardCoords, true)
  lazy val lord = this.characters.find(_.id == 0).get    //There is always a lord.
  lazy val enemyLord = this.game.human.characters.find(_.id == 0).get    //There is always a lord.
  val originalFrontier = this.findFrontier
  var undefendedFrontier = originalFrontier.toBuffer
  var freeRegion = cellCoords.diff(undefendedFrontier)
  var plots = freeRegion
  this.updatePlots()
  var provinces = this.map.provinces.filter(_.owner == Some(this))
  var attacks = 0
  val resourceCalls = Array[Int](0, 0, 0)    //gold, wood, stone
  var tasks = collection.mutable.Stack[String]("prepare", "wood", "wood", "stone", "fortify", "barracks", "evaluate")
  
  def troopValue = this.characters.map(char => Data.specsOfCharacter(char.id)._4).sum
  def valueOf(troops: Seq[Character]): Int = troops.map(char => Data.specsOfCharacter(char.id)._4).sum
  def enemyTroopValue = this.game.human.characters.map(char => Data.specsOfCharacter(char.id)._4).sum
  
  /**
   * Grants the AI an opportunity to act (typically once at every frame).
   */
  def act(): Unit = {
    //println(this.tasks)
    if (game.frameCount % 5 == 0) {
      if (!attacking.isEmpty && this.map.provinces.size == this.provinces.size) {
        this.updateAttackTarget()
      }
    }
    if (game.frameCount % 50 == 0) {
      val newUndefended = this.originalFrontier.diff(this.undefendedFrontier).filter(cell => !this.currentGrid.buildingAt(cell._1, cell._2).isDefined)
      this.undefendedFrontier ++= newUndefended
    }
    if (game.frameCount % (rand.nextInt(30) + 1) == 0) {  //Gives a more natural impression.
      val provincesNow = this.map.provinces.filter(_.owner == Some(this))
      if (provincesNow.size > this.provinces.size) {
        this.provinceGuard = this.provinceGuard ++ this.attacking
        this.attacking = Vector[Character]()
      }
      this.provinces = provincesNow
      val nextTask = tasks.pop
      nextTask match {
        case "prepare" => {this.prepare()}
        case "gold" => {this.increaseGold()}
        case "wood" => {this.increaseWood()}
        case "stone" => {this.increaseStone()}
        case "fortify" => {this.fortify()}
        case "barracks" => {this.barracks()}
        case "siege" => {this.siege()}
        case "evaluate" => {tasks.push("evaluate"); this.evaluate()}
        case "guard" => {this.recruitGuards()}
        case "defence" => {this.recruitDefensive()}
        case "offence" => {this.recruitOffensive()}
        case "attack" => {this.attack()}
        case _ => {}
      }
    }
  }
  
  
  /**
   * Deploys the given character on the frontier, if a frontier has been started.
   */
  def setDefensivePosition(character: Character): Unit = {
    val fortifications = this.buildings.filter(b => b.isFortification && b.isHollow)
    if (!fortifications.isEmpty) {
      val fortification = fortifications(rand.nextInt(fortifications.length))
      fortification.asInstanceOf[Hollow].addCharacters(character)
    }
  }
  
  /**
   * Returns the map posiiton at the given coordinates.
   * If middle, returns the position of the approximate middle of this cell,
   * otherwise the upper left corner.
   */
  private def positionAt(coords: (Int, Int), middle: Boolean) = {
    val constant = if (middle) map.pointsPerCell / 2 else 0
    new Position(coords._1 * map.pointsPerCell + constant, coords._2 * map.pointsPerCell + constant)
  }
  
  /**
   * Executes initial preparations.
   */
  private def prepare() = {
    this.centralPlaza = this.findCentralPlaza
    this.freeRegion = this.freeRegion.diff(this.centralPlaza)
    this.mainStreet = this.findMainStreet
    this.freeRegion = this.freeRegion.diff(this.mainStreet)
    this.lifeGuards = this.characters.filter(_.id != 0)
    this.lord.setTarget(this.lordPosition)
    this.lifeGuards.foreach(_.setTarget(this.guardPosition))
  }
  
  /**
   * Tries to build a building anywhere on the free area.
   */
  private def build(building: Building): Boolean = {
    if (this.canAffordBuilding(building.id)) {
      val spaces = this.plots.toBuffer
      while (!spaces.isEmpty) {
        val coords = spaces(rand.nextInt(spaces.length))
        if (building.shape.map(tuple => (coords._1 + tuple._1, coords._2 + tuple._2))
            .forall(tuple => this.freeRegion.contains(tuple)) &&
            currentGrid.fitsBuilding(coords._1, coords._2, building)) {
          val succeeded = map.build(building, new Position(coords._1 * map.pointsPerCell, coords._2 * map.pointsPerCell))
          if (succeeded) {
            this.updatePlots()
            return true
          }
        }
      }
    } else {
      this.solveResourceProblem(building.id)
    }
    false
  }
  
  private def evaluate() = {
    if (this.valueOf(this.lifeGuards) < this.enemyTroopValue / 3) {
      if (this.buildings.find(_.id == 2).isDefined) {      //Barracks exist
        this.tasks.push("guard")
      } else {
        this.tasks.push("barracks")
      }
    } else if (this.undefendedFrontier.size > 0) {
      this.tasks.push("fortify")
    } else if (this.troopValue > this.enemyTroopValue) {
      if (this.buildings.find(_.id == 2).isDefined) {    //Barracks exist
        this.tasks.push("offence")
      } else {
        this.tasks.push("barracks")
      }
    } else {
      if (this.buildings.find(_.id == 2).isDefined) {    //Barracks exist
        this.tasks.push("defence")
      } else {
        this.tasks.push("barracks")
      }
    }
  }
  
  /**
   * Attempts to increase the gold production.
   */
  private def increaseGold() = {
    val couldBuild = this.build(new House(this))
    if (couldBuild) {
      this.resourceCalls(0) = 0
    } else {
      this.tasks.push("gold")
    }
  }
  
  /**
   * Attempts to incrase the wood production.
   */
  private def increaseWood() = {
    val couldBuild = this.build(new Lumberjack(this))
    if (couldBuild) {
      this.resourceCalls(1) = 0
    } else {
      this.tasks.push("wood")
    }
  }
  
  /**
   * Attempts to increase the stone production.
   */
  private def increaseStone() = {
    val couldBuild = this.build(new Quarry(this))
    if (couldBuild) {
      this.resourceCalls(2) = 0
    } else {
      this.tasks.push("stone")
    }
  }
  
  private def recruitGuards() = {
    this.lifeGuards = (this.lifeGuards.toSet ++ this.characters.toSet).toVector
    this.defensive = Vector[Character]()
    this.offensive = Vector[Character]()
    this.attacking = Vector[Character]()
    this.provinceGuard = Vector[Character]()
    for (guard <- this.lifeGuards) {
      guard.setTarget(this.positionAt(this.guardCoords, true))
    }
    val barracks = this.buildings.find(_.id == 2)
    if (!barracks.isDefined) {
      this.tasks.push("barracks")
    } else {
      val pos = barracks.get.asInstanceOf[Barracks].spawnLocation
      var reqGold = this.valueOf(this.lifeGuards) / 5
      if (this.gold < reqGold) {
        this.solveResourceProblem(reqGold, 0, 0)
      } else {
        while (reqGold >= 0) {
          val troopSelection = Vector(1,1,1,2,2)
          val troop = troopSelection(rand.nextInt(troopSelection.length))
          reqGold -= Data.specsOfCharacter(troop)._4
          if (reqGold >= 0) {
            val soldier = new Soldier(pos, this, troop)
            this.lifeGuards = this.lifeGuards :+ soldier
            soldier.setTarget(this.guardPosition)
          }
        }
      }
    }
  }
  
  private def recruitDefensive() = {
    val troopSelection = Vector(1,1,2)
    val troop = troopSelection(rand.nextInt(troopSelection.length))
    if (this.canAffordUnit(troop)) {
      val barracks = this.buildings.find(_.id == 2)
      if (!barracks.isDefined) {
        this.tasks.push("barracks")
      } else {
        val pos = barracks.get.asInstanceOf[Barracks].spawnLocation
        val soldier = new Soldier(pos, this, troop)
        this.defensive = this.defensive :+ soldier
        this.setDefensivePosition(soldier)
      }
    }
  }
  
  private def recruitOffensive() = {
    if (this.valueOf(this.offensive) > this.enemyTroopValue / 3) {
      this.tasks.push("attack")
    }
    val troopSelection = if (this.buildings.find(_.id == 9).isDefined) Vector(1,1,1,1,1,2,2,5) else Vector(1,1,1,2)
    val troop = troopSelection(rand.nextInt(troopSelection.length))
    if (this.canAffordUnit(troop)) {
      val barracks = this.buildings.find(_.id == 2)
      if (!barracks.isDefined) {
        this.tasks.push("barracks")
      } else {
        val pos = barracks.get.asInstanceOf[Barracks].spawnLocation
        val soldier = new Soldier(pos, this, troop)
        this.offensive = this.offensive :+ soldier
        soldier.setTarget(this.offensivePosition)
      }
    }
  }
  
  /**
   * Sends out the current offensive force in an attack.
   * If there are free provinces to conquer, goes for these.
   * Otherwise, goes directly for the enemy lord.
   */
  private def attack() = {
    this.attacks += 1
    this.attacking = this.attacking ++ this.offensive
    this.offensive = Vector[Character]()
    val provincesToAttack = this.map.provinces.filter(province => !province.owner.isDefined || province.owner.get != this)
    if (provincesToAttack.isEmpty) {
      val target = this.game.human.characters.find(_.id == 0).head.location
      this.attacking.foreach(_.setTarget(target))
    } else {
      val reference = this.frontierCoords
      val province = provincesToAttack.minBy(province =>
        Math.pow(province.flagCoords.get._1 - reference._1, 2) +
        Math.pow(province.flagCoords.get._2 - reference._2, 2))
      val targets = this.currentGrid.neighbours(province.flagCoords.get._1, province.flagCoords.get._2, false)
      val target = targets.find(cell => this.currentGrid.fitsCharacter(cell._1, cell._2, this)).get
      this.attacking.foreach(_.setTarget(this.positionAt(target, true)))
    }
    if (attacks > 5 && !this.buildings.find(_.id == 9).isDefined) {
      this.tasks.push("siege")
    }
  }
  
  private def updateAttackTarget() = {
    val target = this.enemyLord.location
    this.attacking.foreach(_.setTarget(target))
  }
  
  /**
   * Attempts to build barracks, if there are no barracks already.
   */
  private def barracks() = {
    if (!this.buildings.find(_.id == 2).isDefined) {
      if (this.canAffordBuilding(2)) {
        map.build(new Barracks(this), this.barracksPosition)
      } else {
        this.tasks.push("barracks")
        this.solveResourceProblem(2)
      }
    }
  }
  
  /**
   * Attempts to build a siege workshop, if there are no barracks already.
   */
  private def siege() = {
    if (!this.buildings.find(_.id == 9).isDefined) {
      if (this.canAffordBuilding(9)) {
        map.build(new SiegeWorkshop(this), this.siegePosition)
      } else {
        this.tasks.push("siege")
        this.solveResourceProblem(9)
      }
    }
  }
  
  private def solveResourceProblem(buildingId: Int): Unit = {
    val stats = Data.specsOfBuilding(buildingId)
    this.solveResourceProblem(stats._2, stats._3, stats._4)
  }
  
  private def solveResourceProblem(reqGold: Int, reqWood: Int, reqStone: Int): Unit = {
    val gold = reqGold
    val wood = reqWood
    val stone = reqStone
    if (gold > this.gold) {
      this.resourceCalls(0) += 1
      if (this.resourceCalls(1) > 50) {
        this.tasks.push("gold")
      }
    } else if (wood > this.resource("wood")) {
      this.resourceCalls(1) += 1
      if (this.resourceCalls(1) > 10) {
        this.tasks.push("wood")
      }
    } else if (stone > this.resource("stone")) {
      this.resourceCalls(2) += 1
      if (this.resourceCalls(2) > 10) {
        this.tasks.push("stone")
      }
    }
  }
  
  /**
   * Builds the next piece of fortification, if needed.
   * (This includes building fortifications, but not recruting units.)
   */
  private def fortify() = {
    if (!this.buildings.find(_.id == 2).isDefined && this.undefendedFrontier.size > 25) {
      val nextTask = this.tasks.pop()
      this.tasks.push("fortify")
      this.tasks.push(nextTask)
    }
    else {
      this.tasks.push("fortify")
      if (!undefendedFrontier.isEmpty) {
        if (this.canAffordBuilding(5)) {
          val coords = undefendedFrontier.head
            var wall: Option[Building] = None
            if (undefendedFrontier.contains( (coords._1 + 1, coords._2) )) {  //To the right
              if (originalFrontier.contains( (coords._1, coords._2 - 1) ) ||
                  originalFrontier.contains( (coords._1, coords._2 + 1) ) ||
                  originalFrontier.contains( (coords._1 + 1, coords._2 - 1) )||
                  originalFrontier.contains( (coords._1 + 1, coords._2 + 1) )
                  ) {
                wall = Some(new WoodenWallCorner(this))
              } else {
                wall = Some(new WoodenWall(this))
              }
            } else if (undefendedFrontier.contains( (coords._1, coords._2 + 1) )) {  //Down
              if (originalFrontier.contains( (coords._1 - 1, coords._2) ) ||
                  originalFrontier.contains( (coords._1 + 1, coords._2) ) ||
                  originalFrontier.contains( (coords._1 - 1, coords._2 + 1) ) ||
                  originalFrontier.contains( (coords._1 + 1, coords._2 + 1) )
                  ) {
                wall = Some(new WoodenWallCorner(this))
              } else {
                wall = Some(new WoodenWall(this))
                wall.get.horizontal = false
              }
            } else if (undefendedFrontier.contains( (coords._1 - 1, coords._2 )) ||
                       undefendedFrontier.contains( (coords._1, coords._2 - 1) )) {
              /*A wall is going to be built here during another iteration.
              Making sure this cell isn't going to be looked at before its neighbours:*/
              undefendedFrontier -= coords
              undefendedFrontier += coords
            } else {
              wall = Some(new WoodenWallCorner(this))  //Only way to protect this cell is by building a single wall.
            }
            if (wall.isDefined) {
              if (currentGrid.fitsBuilding(coords._1, coords._2, wall.get)) {
                val couldBuild = map.build(wall.get, new Position(coords._1 * map.pointsPerCell, coords._2 * map.pointsPerCell))
                if (couldBuild) {
                  this.undefendedFrontier --= wall.get.coords
                }
              }
          }
        } else {
          this.solveResourceProblem(5)
        }
      }
    }
    if (undefendedFrontier.isEmpty) {
      this.tasks = this.tasks.filter(_ != "fortify")
    }
  }
  
  private def updatePlots() = {
    var result = Vector[(Int, Int)]()
    for (coords <- this.freeRegion) {
      val cell = this.currentGrid.cellAt(coords._1, coords._2)
      if (Terrain.fitsBuilding(cell.terrainType, false) && !cell.building.isDefined) {
        val neighbours = this.originalGrid.neighbours(coords._1, coords._2, false)
        if (neighbours.size < 8 ||  //Looking for corner cells:
            neighbours.find(nb => !Terrain.fitsBuilding(this.currentGrid.cellAt(nb._1, nb._2).terrainType, false)).isDefined ||
            neighbours.find(nb => this.currentGrid.buildingAt(nb._1, nb._2).isDefined).isDefined) {
          result = result :+ coords
        }
      }
    }
    this.plots = result
  }
  
  private def findCentralPlaza = {
    require(!this.freeRegion.isEmpty, "Must have a considerable freeRegion in order to define the central plaza!")
    var result = Vector[(Int, Int)]()
    
    def fits(coords: (Int, Int)): Boolean = {
      for (dx <- 0 until 8; dy <- 0 until 8) {
        val c = (coords._1 + dx, coords._2 + dy)
        if (!this.freeRegion.contains(c)) return false
        else if (!Terrain.fitsBuilding(this.currentGrid.cellAt(c._1, c._2).terrainType, false)) return false
        else if (this.currentGrid.buildingAt(c._1, c._2).isDefined) return false
      }
      true
    }
    
    var found = false
    val candidates = rand.shuffle(this.freeRegion)
    var i = 0
    while (!found && i < candidates.length) {
      val candidate = candidates(i)
      if (fits(candidate)) {
        for (dx <- 0 until 8; dy <- 0 until 8) {
          result = result :+ (candidate._1 + dx, candidate._2 + dy)
        }
        found = true
      }
      i += 1
    }
    result
  }
  
  private def findMainStreet = {
    require(!this.centralPlaza.isEmpty, "Must find a central plaza before defining the main street!")
    val start = this.centralPlaza(21)  //A certain cell in the bottom middle of the central plaza.
    val target = this.originalFrontier.minBy(cell => 
      Math.pow(cell._1 - start._1, 2) + Math.pow(cell._2 - start._2, 2))  //Finds the closest frontier part.
    val path = this.currentGrid.path(start, target, this)    //Finds a path between these.
    path.diff(this.centralPlaza :+ path.last).toVector
  }
  
  private def findFrontier = {
    val border = territory.cellCoords.filter( coords => {
      val neighbours = originalGrid.neighbours(coords._1, coords._2, false)
      if (neighbours.find(!territory.cellCoords.contains(_)).isDefined) true else false
    })
    val criticalBorder = border.filter(coords => {
      if (!currentGrid.fitsCharacter(coords._1, coords._2, game.human)) false
      else if (originalGrid.neighbours(coords._1, coords._2, false).find(coords => {
        !territory.cellCoords.contains(coords) && currentGrid.fitsCharacter(coords._1, coords._2, game.human)
      }).isDefined) true
      else false
    })
    criticalBorder.map(tuple => (tuple._1, tuple._2))
  }
  
}