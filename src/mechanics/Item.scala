package mechanics

class Item(val id: Int) {
  
  /**
   * If the item lies loose on the map, it has its own location, stored in this variable
   * wrapped in an Option. An item can also be located elsewhere, for example in
   * a player's inventory, in which case it lacks actual location and the variable
   * has the value None.
   * 
   * NB! The current version of the game does not fully support items.
   * This is for future use.
   */
  var location: Option[Position] = None
  
  /**
   * Places the item on the given coordinates on the map.
   */
  def place(pos: Position) = {
    this.location = Some(pos)
  }
}