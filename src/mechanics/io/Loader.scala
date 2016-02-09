package mechanics.io

import mechanics._
import scala.io.Source
import java.io.File
import scala.collection.mutable.Buffer


object Loader {
  val mapPrefix = "maps/"
  val mapFormatHead = "AgeOfLords_MapFile"
  val dataPrefix = "data/"
  
  
  
  /**
   * This method loads a map from a map file. More specifically, it takes as parameter a Map file's
   * file name, which it reads and creates a new Map object if the Map file is valid.
   * Map formats are in a human-readable text format but has quite strict requirements. The loadMap
   * function will throw LoadMapException errors explaining what part is corrupted.
   * @param filename The name of the map file.
   */
  def loadMap(filename: String): Map = {
    val file = new File(mapPrefix + filename)
    if (!file.exists()) throw new LoadMapException("Requested map file does not exist.")
    val source = Source.fromFile(file)
    
    var mapDims: (Int, Int, Int) = null
    var terrain: Array[Array[Terrain.Value]] = null
    val territoryBuffer = Buffer[(String, Vector[(Int, Int)], Option[(Int, Int)])]()      // A buffer of tuples: (territotryType: String, cellCoordsContained: Buffer[Cell])
    var humanTerritory: Option[Territory] = None
    var aiTerritory: Option[Territory] = None
    val provinces = Buffer[Territory]()
    
    try {
      val lines = source.getLines.filter( !_.isEmpty).map( _.trim )        //Empty lines are allowed in the file, and extra spaces in the end/beginning of lines.
      if (lines.next() != mapFormatHead) throw new LoadMapException("Requested map file is not a map file.")
      val width = lines.next.split(" ").last.toInt
      val height = lines.next.split(" ").last.toInt
      val pointsPerCell = lines.next.drop(15).toInt
      if (lines.next.trim != "map:") throw new LoadMapException("Map file corrupted.")
      val gridWidth = width / pointsPerCell
      val gridHeight = height / pointsPerCell
      if (gridWidth < 1 || gridHeight < 1) throw new LoadMapException("Invalid map dimensions (no terrain).")
      if (gridWidth * pointsPerCell != width || gridHeight * pointsPerCell != height) throw new LoadMapException("Map dimensions are not divisible with the points per cell.")
      
      val terrainChars = Array.ofDim[Char](gridWidth, gridHeight)
      
      for (y <- 0 until gridHeight) {      //Creates a complete 2-dimensional array describing the entire terrain with Chars representing the terrain types.
        val row = lines.next.split(" ")
        for (x <- 0 until gridWidth) {
          terrainChars(x)(y) = row(x).charAt(0)
        }
      }
      
      //Loads the territories:
      val territoryCorruption = new LoadMapException("Map file corrupted (territories).")
      if (lines.next.trim != "territories:") {throw territoryCorruption}
      var loadingTerritories = true
      while (loadingTerritories) {
        val t = lines.next.trim
        if (t == "{") {
          val typeLine = lines.next.trim
          if (!typeLine.contains("type:")) throw territoryCorruption
          val territoryType = typeLine.split(":").last.trim
          var cellsOrFlag = lines.next.trim
          var flagOption: Option[(Int, Int)] = None
          if (cellsOrFlag.startsWith("flag:")) {
            try {
              val coordsString = cellsOrFlag.split(" ").tail.reduce(_ + _)
              val coordsAsString = coordsString.split(",").map(_.filter(_.isDigit))
              val flagX = coordsAsString(0).toInt
              val flagY = coordsAsString(1).toInt
              flagOption = Some(flagX, flagY)
            } catch {case _: Exception => {throw territoryCorruption}}
            finally {
              cellsOrFlag = lines.next.trim
            }
          }
          if (cellsOrFlag.trim.toLowerCase != "cells:") {throw territoryCorruption}
          val cellCoordBuffer = Buffer[(Int, Int)]()
          var loadingTerritory = true
          while (loadingTerritory) {
            val nextLine = lines.next.trim
            if (nextLine == "}") {
              loadingTerritory = false
            } else {
              val cellCoords = nextLine.drop(1).dropRight(1).split(",").map(_.trim)
              if (cellCoords.length != 2) throw territoryCorruption
              val xValues = cellCoords(0).split(":").map(_.trim.toInt)
              val yValues = cellCoords(1).split(":").map(_.trim.toInt)
              if (!(xValues.length == 1 || xValues.length == 2) || !(yValues.length == 1 || yValues.length == 2)) throw territoryCorruption else {}
              for (x <- xValues(0) to xValues(xValues.length - 1); y <- yValues(0) to yValues(yValues.length - 1)) {
                if (x < 0 || y < 0 || x >= width / pointsPerCell || y >= height / pointsPerCell) {
                  throw new LoadMapException("Map file corrupted (territory coordinates out of bounds).")
                } else {
                  cellCoordBuffer += ((x, y))
                }
              }
              
            }
          }
          territoryBuffer += ((territoryType, cellCoordBuffer.toVector, flagOption))
        } else if (t == "end") {
          loadingTerritories = false
        } else {
          throw territoryCorruption
        }
      }
      val terTypes = territoryBuffer.map(_._1)
      if (!terTypes.contains("player") || !terTypes.contains("ai")) throw new LoadMapException("Player or AI territory missing.")
      
      territoryBuffer.foreach( tuple => {
        if (tuple._1 == "player") {
          humanTerritory = Some(new Territory(tuple._1, tuple._2:_*))
        } else if (tuple._1 == "ai") {
          aiTerritory = Some(new Territory(tuple._1, tuple._2:_*))
        } else {
          if (tuple._1 != "province") throw new LoadMapException("Terrotory type '" + tuple._1 + "' not recognized.")
          val province = new Territory(tuple._1, tuple._2:_*)
          if (tuple._3.isDefined) {province.flagCoords = tuple._3.get}
          provinces += province
        }
      } )
      
      //Checks validity of terrain coordinates and values:
      mapDims = (width, height, pointsPerCell)
      terrain = terrainChars.map( x => x.map( y => Terrain.getValue(y.toString) ) )
      
      if (!terrain.forall( x => x.forall( y => Terrain.exists(y)))) throw new LoadMapException("Map file contains unknown terrain types.")
      
    } catch {
      //Exception handling:
      case e: LoadMapException => throw e
      case e: java.lang.NumberFormatException => throw new LoadMapException("Map file corrupted (number format exception).")
      case e: java.lang.ArrayIndexOutOfBoundsException => throw new LoadMapException("Map file corrupted (index out of bounds).")
      case _: Exception => throw new LoadMapException("Unknown error loading map.")
    } finally {
      source.close()
    }
    
    val map = new Map(mapDims._1, mapDims._2, mapDims._3, humanTerritory.get, aiTerritory.get, provinces.toVector)  //Creates a new Map.
    
    for (x <- 0 until terrain.size; y <- 0 until terrain(0).size) {                                    //Fills the map with its terrain.
      val terrainType = terrain(x)(y)
      map.setTerrain(x, y, terrainType)
    }
    
    map
  }
  
  
  /**
   * Attempts to load character data when starting the game.
   */
  def loadCharacters() = {
    val file = new File(dataPrefix + "characters.txt")
    if (!file.exists()) throw new LoadCharactersException("Requested data file does not exist.")
    val source = Source.fromFile(file)
    var idList = Vector[Int]()
    
    try {
      val lines = source.getLines.filter( !_.isEmpty).map( _.trim )        //Empty lines are allowed in the file, and extra spaces in the end/beginning of lines.
      
      while(lines.hasNext) {
        val line = lines.next
        if (!line.startsWith("id")) {
          val r = line.split("	").filter(!_.isEmpty).map(_.trim).map(_.filter(_.toInt != 9))  //Uses TABS!
          val id = r(0).toInt
          if (!idList.contains(id)) {
            idList = idList :+ id
            val specs = (r(1).toInt, r(2), r(3), r(4).toInt, r(5).toInt, r(6).toInt, r(7).toInt, r(8).toInt)
            Data.addCharacter(id, specs)
            val name = specs._2
            ui.Textures.addCharacter(id, name + ".png", name + "_selected.png", name + "_enemy.png", name + ".png")
          }
        }
      }
      
    } catch {
      //Exception handling:
      case e: LoadCharactersException => throw e
      case e: java.lang.NumberFormatException => throw new LoadCharactersException("Characters file corrupted (number format exception).")
      case e: java.lang.ArrayIndexOutOfBoundsException => throw new LoadCharactersException("Characters file corrupted (index out of bounds).")
      case _: Exception => throw new LoadCharactersException("Unknown error loading map.")
    } finally {
      source.close()
    }
    
    
  }
  
  
  
  /**
   * Attempts to load building data when starting the game.
   */
  def loadBuildings() = {
    val file = new File(dataPrefix + "buildings.txt")
    if (!file.exists()) throw new LoadBuildingsException("Requested data file does not exist.")
    val source = Source.fromFile(file)
    var idList = Vector[Int]()
    
    try {
      val lines = source.getLines.filter( !_.isEmpty).map( _.trim )        //Empty lines are allowed in the file, and extra spaces in the end/beginning of lines.
      
      while(lines.hasNext) {
        val line = lines.next
        if (!line.startsWith("id")) {
          val r = line.split("	").filter(!_.isEmpty).map(_.trim).map(_.filter(_.toInt != 9))  //Uses TABS!
          val id = r(0).toInt
          if (!idList.contains(id)) {
            idList = idList :+ id
            val specs = (r(1), r(2).toInt, r(3).toInt, r(4).toInt, r(5).toInt)
            Data.addBuilding(id, specs)
            val name = specs._2
          }
        }
      }
      
    } catch {
      //Exception handling:
      case e: LoadCharactersException => throw e
      case e: java.lang.NumberFormatException => throw new LoadCharactersException("Characters file corrupted (number format exception).")
      case e: java.lang.ArrayIndexOutOfBoundsException => throw new LoadCharactersException("Characters file corrupted (index out of bounds).")
      case _: Exception => throw new LoadCharactersException("Unknown error loading map.")
    } finally {
      source.close()
    }
    
    
  }
  
  
}

class LoadMapException(msg: String) extends Exception(msg)

class LoadCharactersException(msg: String) extends Exception(msg)

class LoadBuildingsException(msg: String) extends Exception(msg)

class LoadGameException(msg: String) extends Exception(msg)