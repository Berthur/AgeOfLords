package mechanics

/**
 * The terrain object contains information about the different
 * terrains that the map may contain. These may have different
 * properties for building, moving characters etc.
 */
object Terrain extends Enumeration {
  type Terrain = Value
  
  val PLAIN = Value("P")            //Plan terrain with no restrictions.
  val SALTWATER = Value("S")        //Water, cannot be used as a resource.
  val FRESHWATER = Value("F")       //Water, can be used as a resource.
  val OBSTACLE = Value("O")         //Rougher terrain. Only fortifications. Characters.
  val CONSTRUCTION = Value("C")     //Something in the way. No buildings. Characters.
  val ROCK = Value("R")             //Impenetrable terrain. No buildings. No characters.
  val UNKNOWN = Value("U")          //Unknown type. Cannot be loaded.
  
  def getValue(s: String) = s match {
    case "P" => PLAIN
    case "S" => SALTWATER
    case "F" => FRESHWATER
    case "O" => OBSTACLE
    case "C" => CONSTRUCTION
    case "R" => ROCK
    case _ => UNKNOWN
  }
  
  def exists(terrainType: Terrain.Value) = terrainType match {
    case UNKNOWN => false
    case _ => true
  }
  
  def fitsCharacter(terrainType: Terrain.Value) = terrainType match {
    case PLAIN => true
    case OBSTACLE => true
    case CONSTRUCTION => true
    case _ => false
  }
  
  def fitsItem(terrainType: Terrain.Value) = terrainType match {
    case PLAIN => true
    case OBSTACLE => true
    case _ => false
  }
  
  def fitsBuilding(terrainType: Terrain.Value, fortification: Boolean) = terrainType match {
    case PLAIN => true
    case OBSTACLE => fortification    //Only fortifications can be built on this terrain type.
    case _ => false
  }
  
  def fitsWatercraft(terrainType: Terrain.Value) = terrainType match {
    case SALTWATER => true
    case FRESHWATER => true
    case _ => false
  }
  
  def freshwaterSource(terrainType: Terrain.Value) = terrainType match {
    case FRESHWATER => true
    case _ => false
  }
  
}