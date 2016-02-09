package mechanics

import scala.math._

/**
 * Represents a (quantized) 2D coordinate pair.
 */
class Position(val x: Int, val y: Int) {
  
  /**
   * Calculates the distance to another Position.
   */
  def distanceTo(pos: Position): Double = sqrt(pow(pos.x - this.x, 2) + pow(pos.y - this.y, 2))
  
  /**
   * Returns the new Position one would have when travelling for with velocity 'v' for time 'time'.
   */
  def locationAfter(v: Velocity, time: Int): Position = new Position(round((this.x.toDouble + v.x * time)).toInt, round((this.y.toDouble + v.y * time)).toInt)
  
  def velocityTowards(pos: Position, speed: Int): Velocity = {
    val dx = pos.x - this.x
    val dy = pos.y - this.y
    val d = distanceTo(pos)
    val xComponent = dx / d * speed
    val yComponent = dy / d * speed
    new Velocity(xComponent, yComponent)
  }
  
  /**
   * Returns a describing string.
   */
  override def toString = "(" + x + ", " + y + ")"
  
}