package mechanics.ui

import mechanics._
import scala.swing._
import scala.collection.mutable.Buffer
import java.io.File
import javax.imageio.ImageIO
import javax.swing.Icon
import javax.swing.ImageIcon
import java.awt.Color
import java.awt.event._
import javax.sound.sampled._
import java.awt.image.BufferedImage

/**
 * The core (and most components of) the Graphical User Interface.
 */
object GUI1 extends SimpleSwingApplication {
  
  /**
   * The Frames Per Second.
   * The FPS of this game does not simply improve the graphical quality,
   * it has a significance to the speed itself of the game. Recommended:
   * 2 (slow)
   * 4 (normal)
   * 6 (fast)
   * The FPS can be given higher values for testing purposes.
   * NB! A high fps can be very CPU demanding. Slow or normal
   * speed is recommended.
   */
  var FPS = 4
  var PERFORMANCE_CHECK = false    //A debugging tool, to notice fps fluctuations (a stable, slow fps is ideal).
  val rand = new util.Random
  val announcementFeed = new AnnouncementFeed    //Component defined in another file.
  import announcementFeed._                      //For convenience
  val mapFile = "river_frontier"
  val map = io.Loader.loadMap(mapFile + ".txt")
  val game = new Game(map, announcementFeed)
  val player = new Human("Sture", game)
  val opponent = new AI("Sven", game)
  game.setHuman(player)
  game.setAi(opponent)
  private val playerSpawn = rand.shuffle(map.humanTerritory.cellCoords.filter(
      coords => map.gridCurrently.fitsCharacter(coords._1, coords._2, player))).head
  private val opponentSpawn = rand.shuffle(map.aiTerritory.cellCoords.filter(
      coords => map.gridCurrently.fitsCharacter(coords._1, coords._2, opponent))).head
  val playerLord = new Soldier(new Position(
      playerSpawn._1 * map.pointsPerCell + map.pointsPerCell / 2, playerSpawn._2 * map.pointsPerCell + map.pointsPerCell / 2
      ), player, 0)
  val opponentLord = new Soldier(new Position(
      opponentSpawn._1 * map.pointsPerCell + map.pointsPerCell / 2, opponentSpawn._2 * map.pointsPerCell + map.pointsPerCell / 2
      ), opponent, 0)
  var selectedCharacters = Vector[Character]()
  var selectedBuildingType: Option[Building] = None
  val buildingSelections = Array.ofDim[Int](map.gridWidth, map.gridHeight) //Stores info about which cells are selected for a building (0 = not selected, 1 = selected can build, 2 = selected cannot build)
  var selectedBuilding: Option[Building] = None
  var lastSelectedBuilding: Option[Building] = None
  
  var ctrl = false
  var word = ""
  
  val pixelsPerCell = 35 //This is a constant which should NOT be changed unless all the texture resolutions are also changed. NOTE: Not necessarily identical to the points per cell value.
  val emptyIcon = new ImageIcon("textures/empty_35.png")
  val canBuildIcon = new ImageIcon("textures/canBuild_35.png")
  val cannotBuildIcon = new ImageIcon("textures/cannotBuild_35.png")
  val targetIcon = new ImageIcon("textures/flag_35.png")
  val mapImage = ImageIO.read(new File("maps/" + mapFile + ".png"))
  
  
  //-------COMPONENTS:--------
  
  def destroyDialog = {
    val buildingName = lastSelectedBuilding.get.name
    new GameDialog("Destroy building", "Are you sure you want to destroy this " + buildingName + "?", (
      "Yes", () => {
        lastSelectedBuilding.get.destroy(true)
        lastSelectedBuilding = None
        selectedBuilding = None
        resume()
      }
      ), ("Cancel", () => {
        resume()
      }))
  }
  
  val victoryDialog = {
    new GameDialog("VICTORY!", "<html><p>Congratulations!</p><p></p><p>You were victorious.</p></html>",
        ("Accept", () => { GUI1.quit() }) )
  }
  
  val defeatDialog = {
    new GameDialog("DEFEAT!", "<html><p>Disaster!</p><p></p><p>You were defeated.</p></html>",
      ("Accept", () => { GUI1.quit() }) )
  }
  
  val window = new BoxPanel(Orientation.Horizontal) {}
  
  val mainWindow = new BoxPanel(Orientation.Vertical)
  
  val topPanel = new BoxPanel(Orientation.Horizontal) {
    this.background = Color.BLACK
  }
  
  val topMenu = new BoxPanel(Orientation.Horizontal) {
    this.maximumSize = new Dimension(1000, 20)
    this.preferredSize = new Dimension(200, 20)
    this.background = Color.BLACK
    this.contents += new Label("Help | Surrender | Eat Cookies")
    val destroyButton = new Button {
      this.font = new scala.swing.Font("andalus", 0, 12)
      this.text = "Destroy selected building"
      this.name = "destroy"
    }
    this.contents += destroyButton
    this.listenTo(destroyButton)
    this.reactions += {
      case b: scala.swing.event.ButtonClicked => {
        b.source.name match {
          case "destroy" => {
            if (lastSelectedBuilding.isDefined) {
              val dialog = destroyDialog
              pause()
              dialog.open()
            } else {
              announce("You must select the building you wish to destroy!", Color.RED)
            }
          }
        }
      }
    }
  }
  
  val topBar = new BoxPanel(Orientation.Horizontal) {
    val introLabel = new Label {
      this.icon = new ImageIcon("textures/icons/intro1.png")
    }
    val nameLabel = new Label("<html><font color = 'white'>" + player.name + "</font></html>")
    val moneyLabel = new Label {
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/coin2.png")
      this.text = ""
    }
    val coinrateLabel = new Label {
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/coinrate2.png")
      this.text = ""
    }
    val woodLabel = new Label {
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/wood2.png")
      this.text = ""
    }
    val stoneLabel = new Label {
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/stone2.png")
      this.text = ""
    }
    val outroLabel = new Label {
      this.icon = new ImageIcon("textures/icons/outro1.png")
    }
    this.contents += introLabel
    this.contents += nameLabel
    this.contents += moneyLabel
    this.contents += coinrateLabel
    this.contents += woodLabel
    this.contents += stoneLabel
    this.contents += outroLabel
    this.preferredSize = new Dimension(1000, 20)
    this.maximumSize = new Dimension(1000, 20)
    this.minimumSize = new Dimension(1000, 20)
    this.opaque = false
  }
  
  /**
   * Updates the top info bar to correspond with present figures.
   */
  def updateTopBar() = {
    topBar.moneyLabel.text = "<html><font color = '" + (if (player.gold > 0) "yellow" else "red") + "'>" + player.gold.toString + "</font></html>"
    topBar.coinrateLabel.text = "<html><font color = '" + (if (player.goldPerTurn >= 0) "yellow" else "red") + "'>" + player.goldPerTurn.toString + "</font></html>"
    topBar.woodLabel.text = "<html><font color = '" + (if (player.resource("wood") > 0) "yellow" else "red") + "'>" + player.resource("wood").toString + "</font></html>"
    topBar.stoneLabel.text = "<html><font color = '" + (if (player.resource("stone") > 0) "yellow" else "red") + "'>" + player.resource("stone").toString + "</font></html>"
  }
  
  val rightMenu = new BoxPanel(Orientation.Horizontal) {
    val announcementPane = new ScrollPane {
      this.contents = announcementFeed
      this.horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    }
    contents += announcementPane
    def scrollDown() = {    //Scrolls down the pane as much as possible.
      announcementPane.verticalScrollBar.value = announcementPane.verticalScrollBar.maximum
    }
  }
  
  val middlePanel = new BoxPanel(Orientation.Horizontal)
  
  val world = new BoxPanel(Orientation.Horizontal) {
    
    this.maximumSize = new Dimension(map.gridWidth * pixelsPerCell, map.gridHeight * pixelsPerCell)
    
    override def paintComponent(g: Graphics2D) = {
      g.drawImage(mapImage, 0, 0, null)
    }
    
    this.listenTo(mouse.clicks)
    this.listenTo(mouse.wheel)
    this.listenTo(mouse.moves)
    this.listenTo(keys)
    
    this.reactions += {
      //MOUSE CLICKED:
      case mouseClickedEvent: scala.swing.event.MousePressed =>
        if (!timer.isRunning()) {resume()}
        lastSelectedBuilding = None
        val button = mouseClickedEvent.peer.getButton
        val currentGrid = map.createGrid
        val cellX = mouseClickedEvent.point.x / pixelsPerCell
        val cellY = mouseClickedEvent.point.y / pixelsPerCell
        val mapPosition = map.gridCoordsToPosCentered(cellX, cellY)
        if (button == 3) {    //Right mouse button
          val ownSelectedCharacters = selectedCharacters.filter(_.owner == game.human)
          val buildingOption = currentGrid.buildingAt(cellX, cellY)
          if (!ownSelectedCharacters.isEmpty && !buildingOption.isDefined) {
            if (Terrain.fitsCharacter(currentGrid.terrainAt(cellX, cellY))) {
              ownSelectedCharacters.foreach( char => {
                char.setTarget(mapPosition)
                if (char.building.isDefined) char.building.get.asInstanceOf[Hollow].removeCharacters(char)  //Moves the character out of a hollow building.
                } )
              if (currentGrid.cellAt(cellX, cellY).isEmpty) {
                setCharacterTargetTexture(currentGrid)  //Sets the flag immediately, to avoid having to wait for the next frame. This is optional.
              }
            } else {  //Trying to move the characters onto impenetrable terrain.
              announce("Cannot move characters there!", Color.RED)
            }
          } else if (!ownSelectedCharacters.isEmpty && buildingOption.get.isHollow) {
            ownSelectedCharacters.foreach(char => {
              if (char.building.isDefined) char.building.get.asInstanceOf[Hollow].removeCharacters(char)  //Moves the character out of a hollow building.
            } )
            if (ownSelectedCharacters.find(_.owner != buildingOption.get.owner).isDefined) {
              announce("Cannot move troops into an enemy building!", Color.RED)
            }
            val building = buildingOption.get.asInstanceOf[Hollow]
            if (ownSelectedCharacters.find(_.id == 0).isDefined) {
              announce("The Lord cannot hide in a building!", Color.RED)
            }
            building.addCharacters(ownSelectedCharacters:_*)
          }
          if (selectedBuildingType.isDefined) {
            val couldBuild = currentGrid.fitsBuilding(cellX, cellY, selectedBuildingType.get)
            if (couldBuild) {
              map.build(selectedBuildingType.get, mapPosition)
              emptyGridPartially()
              emptyBuildingSelections()
              if (Vector(5, 6, 10, 11).contains(selectedBuildingType.get.id)) {  //For convenience, some buildings have the property of not being deselected after building.
                val newBuilding = Building.createBuilding(
                    selectedBuildingType.get.id, selectedBuildingType.get.owner, selectedBuildingType.get.horizontal)
                if (player.canAffordBuilding(newBuilding.id)) {
                  selectedBuildingType = Some(newBuilding)
                } else {
                  selectedBuildingType = None
                }
              } else {
                selectedBuildingType = None
              }
            } else {
              val territoryOption = currentGrid.territoryAt(cellX, cellY)
              if (territoryOption.isDefined && (!territoryOption.get.owner.isDefined || territoryOption.get.owner.get != player)) {
                announcementFeed.announce("You can only build on your own main territory.", Color.RED)
              } else {
                announcementFeed.announce("You can't build that there!", Color.RED)
              }
            }
          }
        } else if (button == 1 || button == 2) {    //Left/middle mouse button
          val buildingHere = currentGrid.buildingAt(cellX, cellY)
          if (buildingHere.isDefined && buildingHere.get.owner == player) {
            clearAllSelections()
            selectedBuilding = buildingHere
            lastSelectedBuilding = selectedBuilding
          } else {
            val charactersHere = currentGrid.charactersAt(cellX, cellY)
            if (!charactersHere.isEmpty && !ctrl) {
              val amount = if (button == 1) 1 else charactersHere.length / 2 + charactersHere.length % 2  //Takes half the soldiers if middle mouse button (ceiled).
              selectedCharacters = charactersHere.take(amount)
            } else if (!charactersHere.isEmpty && ctrl) {
              selectedCharacters = selectedCharacters.filter(!charactersHere.contains(_)) ++ charactersHere.filter(!selectedCharacters.contains(_))
            } else {
              selectedCharacters = Vector()
            }
            if (!charactersHere.isEmpty && !charactersHere.forall(!selectedCharacters.contains(_))) {
              val characterHere = charactersHere(0)
              setTexture(cellX, cellY, Textures.character(characterHere.id, true, characterHere.owner == player))  //Sets the selected texture without having to wait for the next frame.
            }
            selectedBuilding = None
            emptyBuildingSelections()
            selectedBuildingType = None
            updateBottomMenu()
          }
        }
        
      //MOUSE WHEEL SCROLLED:
      case mouseWheelScrolledEvent: scala.swing.event.MouseWheelMoved =>
        if (selectedBuildingType.isDefined) {
          selectedBuildingType.get.horizontal = !selectedBuildingType.get.horizontal
          setBuildingSelectionTextures(mouseWheelScrolledEvent.point.x, mouseWheelScrolledEvent.point.y)
        }
      //MOUSE MOVED:
      case mouseMovedEvent: scala.swing.event.MouseMoved =>
        setBuildingSelectionTextures(mouseMovedEvent.point.x, mouseMovedEvent.point.y)
        
      //KEY PRESSED:
      case keyPressedEvent: scala.swing.event.KeyPressed =>
        word += keyPressedEvent.peer.getKeyChar()
        if (word.length > 50) word = word.drop(35)
        lastSelectedBuilding = None
        keyPressedEvent.key.id match {
          case 10 => {                                        //Enter
            if (word.filter(_.isLetterOrDigit).toLowerCase.endsWith("pappabetalar")) {
              player.addResource("gold", 10000)
              player.addResource("wood", 1000)
              player.addResource("stone", 1000)
              announce("Daddy pays!", new Color(0x990066))
              resume()
            }
            word = ""
          }
          case 17 => {                                        //Ctrl
            ctrl = true
          }
          case 27 => {                                        //Esc
            clearAllSelections
          }
          case 65 => {                                        //A
            if (ctrl) {
              selectedCharacters = map.characters
              updateBottomMenu()
            }
          }
          case 80 => {                                        //P
            if (timer.isRunning()) pause() else resume()
          }
          case 123 => {                                        //F12
            if (PERFORMANCE_CHECK) {
              PERFORMANCE_CHECK = false
            } else {
              PERFORMANCE_CHECK = true
            }
          }
          
          case _ => {}
        }
        
      //KEY RELEASED:
      case keyReleasedEvent: scala.swing.event.KeyReleased =>
        keyReleasedEvent.key.id match {
          case 17 => {                                          //Ctrl
            ctrl = false
          }
          case _ => {}
        }
        
    }
    focusable = true
    requestFocus
  }
  
  
  
  for (x <- 0 until map.gridWidth) {
    val p = new BoxPanel(Orientation.Vertical)
    for (y <- 0 until map.gridHeight) {
      p.contents += new Label {
        this.verticalTextPosition = Alignment.Bottom
        text = ""
      }
    }
    p.opaque = false
    world.contents += p
  }
  
  val bottomPanel = new BoxPanel(Orientation.Vertical) {}
  
  val bottomMenu = new BoxPanel(Orientation.Horizontal) {
    this.preferredSize = new Dimension(1000, 100)
    override def paintComponent(g: Graphics2D) = {
      g.drawImage(ImageIO.read(new File("bottom_menu.png")), 0, 0, null)
    }
  }
  
  val buildingPanel = new BoxPanel(Orientation.Horizontal) {
    this.opaque = false
    this.maximumSize = new Dimension(1000, 100)
    this.contents += Swing.HStrut(9)
    val buildingButtons = Buffer[Button]()
    for (buildingType <- Data.buildings) {
      buildingButtons += new Button {
        this.name = buildingType.toString
        this.maximumSize = new Dimension(56, 82)
        this.opaque = false
        this.icon = new ImageIcon(Textures.buildingCard(buildingType))
        this.tooltip = "BUILDING!"
        this.listenTo(this.mouse.moves)
        this.reactions += {
          case mouseEntered: scala.swing.event.MouseEntered => {updateBuildingBottomBar(mouseEntered.source.name.toInt)}
          case mouseExited: scala.swing.event.MouseExited => {clearBottomBar()}
        }
      }
    }
    for (button <- buildingButtons) {
      this.contents += button
      this.listenTo(button)
    }
    this.reactions += {
      case b: scala.swing.event.ButtonClicked =>
        if (player.canAffordBuilding(b.source.name.toInt)) {
          selectedBuildingType = Some(Building.createBuilding(b.source.name.toInt, player, true))
        } else {
          announce("You cannot afford this building!", Color.RED)
        }
    }
  }
  
  /**
   * The panel containing all selected units. It is shown inside bottomMenu whenever there are selected units.
   */
  val unitPanel = new BoxPanel(Orientation.Horizontal) {
    this.opaque = false
    this.maximumSize = new Dimension(1000, 100)
    this.contents += Swing.HStrut(9)
    val unitButtons = Buffer[Button]()
    this.reactions += {
      case b: scala.swing.event.ButtonClicked =>
        if (ctrl) {
          selectedCharacters = selectedCharacters.filter(_.id.toString != b.source.name)
        } else {
          selectedCharacters = selectedCharacters.filter(_.id.toString == b.source.name)
        }
        updateBottomMenu()
    }
  }
  
  /**
   * Updates the unit panel (containing all selected units).
   */
  def updateUnitPanel() = {
    val strut = unitPanel.contents(0)
    unitPanel.contents.clear()
    unitPanel.contents += strut
    unitPanel.unitButtons.clear()
    val selectedUnitTypes = selectedCharacters.map(_.id).toSet.toVector.sorted  //Remove duplicates and sort.
    for (unitType <- selectedUnitTypes) {
      unitPanel.unitButtons += new Button {
        this.name = unitType.toString
        this.maximumSize = new Dimension(82, 82)
        this.opaque = false
        this.icon = new ImageIcon(Textures.unitCard(unitType))
        val amount = selectedCharacters.filter(_.id == unitType).length
        if (amount > 1) {
          this.verticalTextPosition = Alignment.Bottom
          this.iconTextGap = - amount.toString.length * 13 - 6    //Makes sure the text fits in the button.
          this.text = "<html><font size = '6' color='CC0000'>" + amount.toString + "</font></html>"
        }
      }
    }
    for (button <- unitPanel.unitButtons) {
      unitPanel.contents += button
      unitPanel.listenTo(button)
    }
    unitPanel.revalidate()
    unitPanel.repaint()
  }
  
  /**
   * The barrack's recruitment menu.
   */
  val barracksPanel = new BoxPanel(Orientation.Horizontal) {
    this.opaque = false
    this.maximumSize = new Dimension(1000, 100)
    this.contents += Swing.HStrut(9)
    val unitButtons = Buffer[Button]()
    for (unitType <- Data.characters.filter(Data.specsOfCharacter(_)._3 == "barracks")) {
      unitButtons += new Button {
        this.name = unitType.toString
        this.maximumSize = new Dimension(82, 82)
        this.opaque = false
        this.icon = new ImageIcon(Textures.unitCard(unitType))
        this.tooltip = "UNIT!"
        this.listenTo(this.mouse.moves)
        this.reactions += {
          case mouseEntered: scala.swing.event.MouseEntered => {updateUnitBottomBar(mouseEntered.source.name.toInt)}
          case mouseExited: scala.swing.event.MouseExited => {clearBottomBar()}
        }
      }
    }
    for (button <- unitButtons) {
      this.contents += button
      this.listenTo(button)
    }
    this.reactions += {
      case b: scala.swing.event.ButtonClicked => {
        val unitType = b.source.name.toInt
        if (player.gold >= Data.specsOfCharacter(unitType)._4) {
          if (player.barrackSpawnLocation.isDefined) {
            player.removeResource("gold", Data.specsOfCharacter(unitType)._4)
            new Soldier(player.barrackSpawnLocation.get, player, unitType)
          } else {
            announcementFeed.announce("It appears you no longer have any barracks.", Color.RED)
          }
        } else {
          announcementFeed.announce("You don't have enough gold to recruit this unit!", Color.RED)
        }
      }
    }
  }
  
  /**
   * The stables' recruitment menu.
   */
  val stablesPanel = new BoxPanel(Orientation.Horizontal) {
    this.opaque = false
    this.maximumSize = new Dimension(1000, 100)
    this.contents += Swing.HStrut(9)
    val unitButtons = Buffer[Button]()
    for (unitType <- Data.characters.filter(Data.specsOfCharacter(_)._3 == "stables")) {
      unitButtons += new Button {
        this.name = unitType.toString
        this.maximumSize = new Dimension(82, 82)
        this.opaque = false
        this.icon = new ImageIcon(Textures.unitCard(unitType))
        this.tooltip = "UNIT!"
        this.listenTo(this.mouse.moves)
        this.reactions += {
          case mouseEntered: scala.swing.event.MouseEntered => {updateUnitBottomBar(mouseEntered.source.name.toInt)}
          case mouseExited: scala.swing.event.MouseExited => {clearBottomBar()}
        }
      }
    }
    for (button <- unitButtons) {
      this.contents += button
      this.listenTo(button)
    }
    this.reactions += {
      case b: scala.swing.event.ButtonClicked => {
        val unitType = b.source.name.toInt
        if (player.gold >= Data.specsOfCharacter(unitType)._4) {
          if (player.stableSpawnLocation.isDefined) {
            player.removeResource("gold", Data.specsOfCharacter(unitType)._4)
            new Soldier(player.stableSpawnLocation.get, player, unitType)
          } else {
            announcementFeed.announce("It appears you no longer have any stables.", Color.RED)
          }
        } else {
          announcementFeed.announce("You don't have enough gold to recruit this unit!", Color.RED)
        }
      }
    }
  }
  
  /**
   * The siege workshop's recruitment menu.
   */
  val siegePanel = new BoxPanel(Orientation.Horizontal) {
    this.opaque = false
    this.maximumSize = new Dimension(1000, 100)
    this.contents += Swing.HStrut(9)
    val unitButtons = Buffer[Button]()
    for (unitType <- Data.characters.filter(Data.specsOfCharacter(_)._3 == "siege workshop")) {
      unitButtons += new Button {
        this.name = unitType.toString
        this.maximumSize = new Dimension(82, 82)
        this.opaque = false
        this.icon = new ImageIcon(Textures.unitCard(unitType))
        this.tooltip = "UNIT!"
        this.listenTo(this.mouse.moves)
        this.reactions += {
          case mouseEntered: scala.swing.event.MouseEntered => {updateUnitBottomBar(mouseEntered.source.name.toInt)}
          case mouseExited: scala.swing.event.MouseExited => {clearBottomBar()}
        }
      }
    }
    for (button <- unitButtons) {
      this.contents += button
      this.listenTo(button)
    }
    this.reactions += {
      case b: scala.swing.event.ButtonClicked => {
        val unitType = b.source.name.toInt
        if (player.gold >= Data.specsOfCharacter(unitType)._4) {
          if (player.siegeSpawnLocation.isDefined) {
            player.removeResource("gold", Data.specsOfCharacter(unitType)._4)
            new Soldier(player.siegeSpawnLocation.get, player, unitType)
          } else {
            announcementFeed.announce("It appears you no longer have any siege workshop.", Color.RED)
          }
        } else {
          announcementFeed.announce("You don't have enough gold to recruit this unit!", Color.RED)
        }
      }
    }
  }
  
  /**
   * Updates the bottom menu to display either building options or selected troops.
   */
  def updateBottomMenu(): Unit = {
    if (selectedBuilding.isDefined) {
      if (selectedBuilding.get.isHollow) {    //May contain troops
        selectedCharacters = selectedBuilding.get.asInstanceOf[Hollow].characters
        selectedBuilding = None
        updateBottomMenu()
      }
      else {
        selectedBuilding.get.id match {
          case 2 => {bottomMenu.contents(0) = barracksPanel}    //Barracks
          case 7 => {bottomMenu.contents(0) = stablesPanel}     //Stables
          case 9 => {bottomMenu.contents(0) = siegePanel}       //Siege workshop
          case _ => {}
        }
      }
    } else if (!selectedCharacters.isEmpty) {
      updateUnitPanel()
      bottomMenu.contents(0) = unitPanel
    } else if (!bottomMenu.contents.contains(buildingPanel)) {
      bottomMenu.contents(0) = buildingPanel
    }
    bottomMenu.repaint()
  }
  
  /**
   * A narrow, horizontal bar over the bottom menu, displaying info about what the mouse is hovering over.
   */
  val bottomBar = new BoxPanel(Orientation.Horizontal) {
    val introLabel = new Label {
      this.icon = new ImageIcon("textures/icons/intro1.png")
    }
    val nameLabel = new Label {visible = false}
    val moneyLabel = new Label {
      visible = false
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/coin2.png")
    }
    val woodLabel = new Label {
      visible = false
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/wood2.png")
    }
    val stoneLabel = new Label {
      visible = false
      this.minimumSize = new Dimension(100, 20)
      this.icon = new ImageIcon("textures/icons/stone2.png")
    }
    val strut = Swing.HStrut(930)
    val outroLabel = new Label {
      this.icon = new ImageIcon("textures/icons/outro1.png")
    }
    
    this.contents += introLabel
    this.contents += nameLabel
    this.contents += moneyLabel
    this.contents += woodLabel
    this.contents += stoneLabel
    this.contents += strut
    this.contents += outroLabel
    this.preferredSize = new Dimension(1000, 20)
    this.maximumSize = new Dimension(1000, 20)
    this.minimumSize = new Dimension(1000, 20)
    this.background = Color.BLACK
  }
  
  def clearBottomBar() = {
    bottomBar.nameLabel.visible = false
    bottomBar.moneyLabel.visible = false
    bottomBar.woodLabel.visible = false
    bottomBar.stoneLabel.visible = false
    bottomBar.strut.visible = true
  }
  
  /**
   * Updates the black bottom bar to show information about the building type currently hovered over.
   */
  def updateBuildingBottomBar(buildingId: Int) = {
    for (label <- bottomBar.contents) {
      label.visible = true
    }
    bottomBar.strut.visible = false
    bottomBar.nameLabel.text = "<html><font color = 'white'>" + Data.specsOfBuilding(buildingId)._1.toUpperCase + "</font></html>"
    bottomBar.moneyLabel.text = "<html><font color = '" + (if (player.gold >= Data.specsOfBuilding(buildingId)._2) "yellow" else "red") + "'>" + Data.specsOfBuilding(buildingId)._2 + "</font></html>"
    bottomBar.woodLabel.text = "<html><font color = '" + (if (player.resource("wood") >= Data.specsOfBuilding(buildingId)._3) "yellow" else "red") + "'>" + Data.specsOfBuilding(buildingId)._3 + "</font></html>"
    bottomBar.stoneLabel.text = "<html><font color = '" + (if (player.resource("stone") >= Data.specsOfBuilding(buildingId)._4) "yellow" else "red") + "'>" + Data.specsOfBuilding(buildingId)._4 + "</font></html>"
  }
  
  /**
   * Updates the black bottom bar to show information about the unit type currently hovered over.
   */
  def updateUnitBottomBar(unitId: Int) = {
    bottomBar.nameLabel.visible = true
    bottomBar.moneyLabel.visible = true
    bottomBar.strut.visible = false
    bottomBar.nameLabel.text = "<html><font color = 'white'>" + Data.specsOfCharacter(unitId)._2.toUpperCase + "</font></html>"
    bottomBar.moneyLabel.text = "<html><font color = '" + (if (player.gold >= Data.specsOfCharacter(unitId)._4) "yellow" else "red") + "'>" + Data.specsOfCharacter(unitId)._4 + "</font></html>"
  }
  
  
  //---------COMPONENT HIERARCHY:---------
  bottomPanel.contents += bottomBar
  bottomPanel.contents += bottomMenu
  bottomMenu.contents += buildingPanel
  middlePanel.contents += rightMenu
  topPanel.contents += topMenu
  topPanel.contents += topBar
  mainWindow.contents += topPanel
  mainWindow.contents += middlePanel
  mainWindow.contents += world
  mainWindow.contents += bottomPanel
  window.contents += mainWindow
  window.contents += rightMenu
  

  //-------SET/RESET FUNCTIONS-------
  
  private def emptyGrid(): Unit = {
    for (x <- 0 until map.gridWidth; y <- 0 until map.gridHeight) {
      world.contents(x).asInstanceOf[BoxPanel].contents(y).asInstanceOf[Label].icon = emptyIcon
    }
  }
  
  private def emptyGridPartially(): Unit = {
    for (x <- 0 until map.gridWidth; y <- 0 until map.gridHeight) {
      val icon = world.contents(x).asInstanceOf[BoxPanel].contents(y).asInstanceOf[Label].icon
      if (icon == canBuildIcon || icon == cannotBuildIcon) {
        world.contents(x).asInstanceOf[BoxPanel].contents(y).asInstanceOf[Label].icon = emptyIcon
      }
    }
  }
  
  private def emptyBuildingSelections(): Unit = {
    for (x <- 0 until map.gridWidth; y <- 0 until map.gridHeight) {
      buildingSelections(x)(y) = 0
    }
  }
  
  def clearAllSelections(): Unit = {
    selectedCharacters = Vector[Character]()
    selectedBuildingType = None
    emptyBuildingSelections
    updateBottomMenu()
  }
  
  private def setTexture(x: Int, y: Int, imageName: String): Unit = {
    world.contents(x).asInstanceOf[BoxPanel].contents(y).asInstanceOf[Label].icon = new ImageIcon(imageName)
  }
  
  private def setTexture(x: Int, y: Int, icon: Icon): Unit = {
    world.contents(x).asInstanceOf[BoxPanel].contents(y).asInstanceOf[Label].icon = icon
  }
  
  private def setText(x: Int, y: Int, text: String): Unit = setText(x, y, text, "yellow")
  
  private def setText(x: Int, y: Int, text: String, colour: String): Unit = {
    val label = world.contents(x).asInstanceOf[BoxPanel].contents(y).asInstanceOf[Label]
    if (label.text != text) {
      label.text = "<html><font color='" + colour + "'>" + text + "</font></html>"
      label.iconTextGap = -pixelsPerCell * text.length / 4          //Places the text on the lower right, but makes sure it fits inside the label.
    }
  }
  
  /**
   * Sets the marker textures for the current building selection
   * (independently - without having to wait for the next frame)
   */
  private def setBuildingSelectionTextures(pixelsX: Int, pixelsY: Int) = {
        if (selectedBuildingType.isDefined) {
          emptyBuildingSelections()
          val cellX = pixelsX / pixelsPerCell
          val cellY = pixelsY / pixelsPerCell
          var value = 2
          if (map.createGrid.fitsBuilding(cellX, cellY, selectedBuildingType.get)) {
            value = 1
          }
          for (part <- selectedBuildingType.get.shape) {
            val x = part._1 + cellX
            val y = part._2 + cellY
            if (x >= 0 && x < map.gridWidth && y >= 0 && y < map.gridHeight) {
              buildingSelections(part._1 + cellX)(part._2 + cellY) = value
            }
            emptyGridPartially()
            updateGUIPartially()
          }
        }
  }
  
  private def setCharacterTargetTexture(currentGrid: Grid) = {
    if (!selectedCharacters.isEmpty && selectedCharacters(0).target.isDefined) {
      val targetPos = selectedCharacters(0).target.get
      if (currentGrid.cellAt(targetPos).isEmpty) {
        setTexture(targetPos.x / map.pointsPerCell, targetPos.y / map.pointsPerCell, targetIcon)
      }
    }
  }
  
  
  //-------APPLICATION:-------
  
  emptyGrid()
  
  def top = new MainFrame {
    contents = window
  }
  
  
  // <<<<<<TESTING>>>>>>  
  for (x <- 0 to 10) {
    new Soldier(player.characters.find(_.id == 0).get.location, player, 1)
  }
  for (x <- 0 to 20) {
    new Soldier(opponent.lord.location, opponent, 1)
  }
  //<<<<<<------->>>>>>
  
  
  /*
   * Updates the entire GUI to describe the up-to-date map data.
   */
  def updateGUI(): Unit = {
    updateTopBar()
    emptyGrid()
    val grid = map.createGrid
    for (x <- 0 until grid.width; y <- 0 until grid.height) {
      setText(x, y, "")
      val building = grid.buildingAt(x, y)
      val part = grid.buildingPartAt(x, y)
      val characters = grid.charactersAt(x, y)
      if (building.isDefined) {
        val texture = Textures.buildingPart(building.get, part.get)
        setTexture(x, y, texture)
        if (building.get.isHollow && (x, y) == building.get.representativeCoords) {  //Check if this is the representative part of the building.
          val charAmount = building.get.asInstanceOf[Hollow].characters.size
          if (charAmount > 0) setText(x, y, charAmount.toString)    //Display the amount of characters the building contains.
        }
      } else if (buildingSelections(x)(y) != 0) {
        val texture = (if (buildingSelections(x)(y) == 1) canBuildIcon else cannotBuildIcon)
        setTexture(x, y, texture)
      } else if (!characters.isEmpty) {
        val character = characters(0)
        val selected = !characters.forall(!selectedCharacters.contains(_))
        val texture = Textures.character(character.id, selected, character.owner == player)
        val amount = characters.length
        val text = {
          val maxHp = Data.specsOfCharacter(character.id)._6
          if (amount > 1) amount.toString
          else if (character.hp < maxHp) "°" * Math.max(1, (5 * character.hp / maxHp))
          else ""
        }
        setTexture(x, y, texture)
        val colour = if (text.contains("°")) (text.length match {
          case 1 => "ff0000"
          case 2 => "ff9100"
          case 3 => "ffd500"
          case 4 => "0091ff"
          case 5 => "00ff00"
        }) else "yellow"
        setText(x, y, text, colour)
      }
      val relevantProvinces = this.map.provinces.filter(_.flagCoords.isDefined)
      if (relevantProvinces.map(_.flagCoords.get).contains((x, y))) {
        val province = relevantProvinces.find(_.flagCoords.get == (x, y)).get  //All provinces have a flag.
        val time = province.time
        if (province.owner.isDefined || province.time != Data.provinceCooldown) {
          val colour = if (!province.owner.isDefined) "00c4ff" else if (province.owner.get == player) "00ff00" else "red"
          setText(x, y, time.toString, colour)
        }
      }
    }
    world.requestFocus
    setCharacterTargetTexture(grid)    //Additional texture updates
  }
  
  /*
   * Does the necessary real-time updates in the GUI for the building placement mechanism to work.
   */
  def updateGUIPartially(): Unit = {
    emptyGridPartially()
    val currentGrid = map.createGrid
    for (x <- 0 until map.gridWidth; y <- 0 until map.gridHeight) {
      if (!currentGrid.buildingAt(x, y).isDefined) {
        if (buildingSelections(x)(y) == 1) {
          setTexture(x, y, canBuildIcon)
        } else if (buildingSelections(x)(y) == 2) {
          setTexture(x, y, cannotBuildIcon)
        }
      }
    }
    world.repaint()
  }
  
  
  rightMenu.scrollDown()
  this.updateGUI()
  Audio.startGameMusic()
  
  /**
   * The timer that drives the game. First parameter defines frame frequency.
   * Higher frame frequency speeds up the game.
   * Recommended: 500 (slow), 375 (medium) or 250 (fast).
   */
  val timer: javax.swing.Timer = new javax.swing.Timer(1000 / FPS, Swing.ActionListener(e => {
    val battle = map.characters.find(_.inBattle).isDefined
    if (battle && !Audio.fightSoundPlaying) {
      Audio.startFightSound()
    } else if (!battle && Audio.fightSoundPlaying) {
      Audio.stopFightSound()
    }
    if (PERFORMANCE_CHECK) {Audio.playSound(Audio.beep)}
    if (!game.isOver) {
      game.frame()
      this.emptyGrid()
      this.updateGUI
      world.repaint()
      updateBottomMenu()  // This is optional but it reduces bottom menu lag if it can be afforded.
    } else {
      pause()
      if (player.isAlive) {
        victory()
      } else {
        defeat()
      }
    }
  }))
  timer.start()
  
  
  def pause(): Unit = {
    if (!game.isOver) {announce("Game paused.", Color.BLACK); Audio.stopGameMusic()}
    timer.stop()
  }
  
  def resume(): Unit = {
    if (!game.isOver) {
      timer.start()
      announce("Game resumes.", Color.BLACK)
      Audio.startGameMusic()
    }
  }
  
  def victory(): Unit = {
    this.pause()
    victoryDialog.open()
    Audio.stopFightSound()
    Audio.stopGameMusic()
    Audio.playSound(new File("sound/music/victory_music.wav"))
  }
  
  def defeat(): Unit = {
    this.pause()
    defeatDialog.open()
    Audio.stopFightSound()
    Audio.stopGameMusic()
    Audio.playSound(new File("sound/music/defeat_music.wav"))
  }
  
}