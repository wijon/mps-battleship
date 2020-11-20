


//case class Settings()
//abstract class Player(ownBoard: Board, enemyBoard: Board)
//case class HumanPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)
//abstract class AiPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)
//case class EasyAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)
//case class MediumAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)
//case class DifficultAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)

import dataTransferObjects.{Coordinates, Ship, ShipPosition}
import enums.{BoardDirection, Player, Player1, Player2}
import enums.BoardDirection.BoardDirection

import scala.collection.mutable

case class Game1() {
  def ships(f: ShipPlacement => Unit): Game1 = {
    val placement = new ShipPlacement
    f(placement)

    Game1(placement.shipPositionsPlayer1.toVector, placement.shipPositionsPlayer2.toVector)

    this
  }
}

object Game1 {
  def apply(pos1: Vector[ShipPosition], pos2: Vector[ShipPosition]): Game1 = {
    new Game1
  }

  def newGame(f: Game1 => Unit): Game1 = {
    val game = new Game1
    f(game)
    game
  }
}

class ShipPlacement {
  val shipPositionsPlayer1: mutable.HashSet[ShipPosition] = mutable.HashSet.empty
  val shipPositionsPlayer2: mutable.HashSet[ShipPosition] = mutable.HashSet.empty

  def place(name: String): ShipPlacementLength = new ShipPlacementLength(shipPositionsPlayer2, name)

  class ShipPlacementLength(val positions: mutable.HashSet[ShipPosition], val name: String) {
    def length(len: Int): ShipPlacementAt = new ShipPlacementAt(positions, name, len)
  }

  class ShipPlacementAt(val positions: mutable.HashSet[ShipPosition], val name: String, val len: Int) {
    // Use Coordinates class with implicit conversion from int to Coordinates
    def at(coord: Int): ShipPlacementFacing = new ShipPlacementFacing(positions, name, len, coord)
  }

  class ShipPlacementFacing(val positions: mutable.HashSet[ShipPosition], val name: String, val len: Int, val coord: Int) {
    def facing(dir: BoardDirection): ShipPlacementPlayer = {
      val strCoord = coord.toString
      val row = strCoord.slice(0, 1).toInt
      val col = strCoord.slice(1, 2).toInt

      val coordinates = dir match {
        case BoardDirection.North =>
          for (r <- row until row - len by -1) yield Coordinates(r, col)
        case BoardDirection.East =>
          for (c <- col until col + len) yield Coordinates(row, c)
        case BoardDirection.South =>
          for (r <- row until row + len) yield Coordinates(r, col)
        case BoardDirection.West =>
          for (c <- col until col - len by -1) yield Coordinates(row, c)
      }

      val ship = Ship(len, name)
      new ShipPlacementPlayer(dataTransferObjects.ShipPosition(ship, Vector.from(coordinates)))
    }
  }

  class ShipPlacementPlayer(shipPosition: ShipPosition){
    def as(player: Player) : Unit = {
      player match {
        case Player1 => shipPositionsPlayer1 += shipPosition
        case Player2 => shipPositionsPlayer2 += shipPosition
      }
    }
  }
}


import Game1._

val game = newGame { game =>

  game ships { ship =>
    ship place "Submarine" length 3 at 14 facing BoardDirection.West as Player1
    ship place "Submarine" length 5 at 14 facing BoardDirection.West as Player1
    ship place "Submarine" length 4 at 14 facing BoardDirection.West as Player2
  }
}