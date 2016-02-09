package mechanics

import scala.math._

/**
 * Represents a 2D velocity in vector form. Contains both direction and magnitude.
 */
class Velocity(val x: Double, val y: Double) {
  
  /**
   * The vector's norm, or the absolute speed of the velocity.
   */
  val speed = sqrt(pow(x, 2) + pow(y, 2))
  
  /**
   * Tells if the velocity is zero.
   */
  def isZero = x == 0 && y == 0
  
  /**
   * Returns a describing string.
   */
  override def toString = "[" + x + ", " + y + "]"
}

/**
 * Companion object with a constructor for the default, zero, velocity.
 */
object Velocity {
  def apply() = new Velocity(0, 0)
}