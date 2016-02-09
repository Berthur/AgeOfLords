package tests

import mechanics._
import mechanics.io._
import scala.io.Source
import java.io.File

/**
 * NB! This object may be completely disregarded.
 * (Only included because I was specifically asked to include all
 * the old tests).
 */
object GeneralTesting extends App {
  
  
  
/*
  /* The test below tests loading a map, creating a game with its
   * territories, tests the player's economy and the building of a
   * building.
   * NB! This tests the game in its VERY early state - 
   * will not work on the current version.
   */
  val map = Loader.loadMap("testmap3.txt")
  val game = new Game(map)
  val player = new Human("Sture", game)
  val opponent = new AI("Sven", game)
  
  val g = map.createGrid
  var i = Array[Cell]()
  for (x <- 0 until 10; y <- 0 until 10) {
    i = i :+ g.cellAt(x, y)
  }
  //val t = new Territory(i:_*)
  
  println("Initial gold: " + player.resource("gold"))
  player.addResource("gold", 5000)
  opponent.addResource("gold", 5000)
  
  println("Building could be built: " + map.build(new DerpBuilding(player), new Position(200, 2000)))
  
  for (x <- 0 to 100) {
    print(player.resource("gold") + ", ")
    map.frame()
  }
  println("")
  
  println(map.createGrid)
  
  println("Testytest test test! :)")
  println("!")
*/
  
  
  
  
  
 
  
/*
  /*
   * The following tests loading a map, setting terrain,
   * creating a game and the building of buildings.
   * NB! This tests the game in its VERY early state - 
   * will not work on the current version.
   */
	val g = Loader.loadMap("testmap.txt").createGrid

  println(g)*/
  
  /*val m = new Map(100, 100, 10)
  for (x <- 0 until 10; y <- 0 until 10) {
    m.setTerrain(x, y, Terrain.PLAIN)
  }
  
  val game = new Game(m)
  
  val h = new Human("Sture", game)
  
  val w = new Watchtower(h, true)
  var g = m.createGrid
  println(g)
  println("Can add watchtower at (0, 0): " + g.fitsBuilding(0, 0, w))
  println("Managed to add watchtower at (0, 0): " + m.build(w, new Position(0, 0)))
  g = m.createGrid
  println(g.buildingAt(0, 0))
  println(g)
  println("------------------------------")
  val d = new DerpBuilding(h, false)
  println("Can add derp building at (5, 4): " + g.fitsBuilding(5, 4, d))
  println("Managed to build derp building at (5, 4): " + m.build(d, new Position(50, 40)))
  g = m.createGrid
  println(g)
  val w2 = new Watchtower(h, true)
  println("Can add watchtower2 at (1, 1): " + g.fitsBuilding(1, 1, w2))
*/

}
