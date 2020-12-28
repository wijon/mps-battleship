package dsl.intern

import dataTransferObjects.{Coordinates, Ship, ShipPosition}
import enums.{BoardDirection, Player}
import model.Board

import scala.collection.mutable

class ShipPlacement {
  val shipPositionsPlayer1: mutable.HashSet[ShipPosition] = mutable.HashSet.empty
  val shipPositionsPlayer2: mutable.HashSet[ShipPosition] = mutable.HashSet.empty

  def place(name: String): ShipPlacementLength = new ShipPlacementLength(name)

  class ShipPlacementLength(val name: String) {
    def length(len: Int): ShipPlacementAt = new ShipPlacementAt(name, len)
  }

  class ShipPlacementAt(val name: String, val len: Int) {
    // Use Coordinates class with implicit conversion from int to Coordinates
    def at(coord: Int): ShipPlacementFacing = new ShipPlacementFacing(name, len, coord)
  }

  class ShipPlacementFacing(val name: String, val len: Int, val coord: Int) {
    def facing(dir: BoardDirection): ShipPlacementPlayer = {
      val strCoord = coord.toString
      val row = strCoord.slice(0, 1).toInt
      val col = strCoord.slice(1, 2).toInt

      val coordinates = Board.generateShipCoordinates(Coordinates(row, col), len, dir)

      val ship = Ship(len, name)
      new ShipPlacementPlayer(dataTransferObjects.ShipPosition(ship, coordinates))
    }
  }

  class ShipPlacementPlayer(shipPosition: ShipPosition){
    def as(player: Player) : Unit = {
      player match {
        case Player.Player1 => shipPositionsPlayer1 += shipPosition
        case Player.Player2 => shipPositionsPlayer2 += shipPosition
      }
    }
  }
}
