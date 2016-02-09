package mechanics

import Node._

/**
 * The Node class represents the nodes used by the character pathfinding included
 * in the Grid class. One node approximately represents the middle of a grid cell.
 * The algoritm used is a variation of the A* pathfinding algorithm.
 * Here, efficiency is more important than mathematical perfection so the
 * pathfinding algorithm used is not ideal.
 */
class Node(val x: Int, val y: Int, var parent: Option[Node], start: (Int, Int), target: (Int, Int)) {
  
  var g = 0      //Distance across nodes to the start coordinates.
  var h = 0      //Approximated straight distance to the target.
  updateG()
  updateH()
  def f = g + h  //The sum of g and h.
  
  /**
   * Updates the g value recursively.
   */
  def updateG() = {
    if (parent.isDefined) {
      if (parent.get.x == this.x || parent.get.y == this.y) {  //Horizontal or vertical.
        g = parent.get.g + horizontalD
      } else {                                                 //Diagonal.
        g = parent.get.g + diagonalD
      }
    } else {
      g = 0                                                    //This is the start node.
    }
  }
  
  /**
   * Updates the h value. This is only a very crude approximation of the distance to
   * the target, but since the maps aren't so large, the efficiency this comes with
   * is more important than always finding the very fastest path.
   */
  def updateH() = {
    (Math.abs(target._1 - this.x) + Math.abs(target._2 - this.y)) * horizontalD
  }
  
  /**
   * Updates all node values.
   */
  def updateValues(): Unit = {
    updateG()
    updateH()
  }
  
  /**
   * Returns this node's grid coordinates in the form of a tuple: (x, y).
   */
  def toCoords = (this.x, this.y)
  
}

/**
 * A companion object to the Node class.
 * Contains some constans for the pathfinding algorithm.
 */
object Node {
  val horizontalD = 10  //An approximation of the relative distance between two adjacent nodes.
  val diagonalD = 14    //An approximation of the relative distance between two diagonal nodes.
}
