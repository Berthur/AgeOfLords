package mechanics

/*
 * This class represents a territory on the map. A territory has a predefined
 * area (defined in GRID COORDINATES, for the sake of simplicity), an owner -
 * which can change - and a fixed type:
 * 
 * - 	Player territory: This territory is always owned by the human player.
 * - 	AI territory: This territory is always owned by the AI player.
 * - 	Province territory: This territory is simpler. It typically starts
 * 		with no owner at all but can be conquered by players multiple times.
 * 		No buildings can be constructed here but it typically generates resources
 * 		for its owner.
 */
class Territory(val territoryType: String, cellsCoordsIncluded: (Int, Int)*) {
  private var ownerOption: Option[Player] = None
  private var cellList = cellsCoordsIncluded
  var flagCoordsOption: Option[(Int, Int)] = None
  var time = Data.provinceCooldown
  
  def hasOwner = ownerOption.isDefined
  def owner = ownerOption
  def owner_=(newOwner: Player) = {ownerOption = Some(newOwner)}
  def flagCoords = flagCoordsOption
  def flagCoords_=(newCoords: (Int, Int)) = {
    require(cellList.contains(newCoords), "The flag must be inside the territory!")
    flagCoordsOption = Some(newCoords)
    }
  def cellCoords = cellList.toVector
  def size = cellList.size
  override def toString() = "Territory of type " + territoryType
}