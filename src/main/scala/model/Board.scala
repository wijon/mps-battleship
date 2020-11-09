package model

import scala.util.{Failure, Success, Try}
import model.BoardDirection.BoardDirection

case class Board(matrix: Vector[Vector[BoardCell]], ships: Vector[Ship], shipPositions: Vector[ShipPosition]) {
  // Override constructor, this one is used for the initial instantiation
  def this(ships: Vector[Ship]) = this(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, ships, Vector())

  /** Generates a vector of coordinates, starting at currentCoordinates moving remainingMoves times into movingDirection
   *
   * @param currentCoordinates current coordinates
   * @param remainingMoves     remaining iterations
   * @param movingDirection    direction to move
   * @return list of generated coordinates
   */
  def generateCoordinates(currentCoordinates: Coordinates,
                          remainingMoves: Int,
                          movingDirection: BoardDirection): Try[Vector[Coordinates]] = {
    if (remainingMoves <= 0) {
      Success(Vector.empty)
    } else {
      moveOneField(currentCoordinates, movingDirection) match {
        case Success(value) => {
          generateCoordinates(value, remainingMoves - 1, movingDirection) match {
            case Success(value) => Success(value :+ currentCoordinates)
            case Failure(ex) => Failure(ex)
          }
        }
        case Failure(ex) => Failure(ex)
      }
    }
  }

  /** moves one field from coordinates into movingDirection and gives new coordinates
   *
   * @param coordinates     coordinates from where to move
   * @param movingDirection moving direction
   * @return new coordinates
   */
  private def moveOneField(coordinates: Coordinates, movingDirection: BoardDirection): Try[Coordinates] = {
    movingDirection match {
      case BoardDirection.North => Success(Coordinates(coordinates.row - 1, coordinates.col))
      case BoardDirection.East => Success(Coordinates(coordinates.row, coordinates.col + 1))
      case BoardDirection.South => Success(Coordinates(coordinates.row + 1, coordinates.col))
      case BoardDirection.West => Success(Coordinates(coordinates.row, coordinates.col - 1))
      case _ => Failure(new NotImplementedError)
    }
  }

  /** Places a single ship on board
   *
   * @param ship        ship to place
   * @param startingRow row-index of first ship field
   * @param startingCol column-index of first ship field
   * @param direction   direction to place
   * @return updated board
   */
  def placeSingleShip(ship: Ship,
                      startingRow: () => Int,
                      startingCol: () => Int,
                      direction: () => BoardDirection): Try[Board] = {
    placeSingleShip(ship, Coordinates(startingRow(), startingCol()), direction())
  }

  /** Places a single ship on board
   *
   * @param ship        ship to place
   * @param coordinates starting coordinates of ship
   * @param direction   direction to place
   * @return updated board
   */
  def placeSingleShip(ship: Ship, coordinates: Coordinates, direction: BoardDirection): Try[Board] = {
    val startRow = {
      if (direction == BoardDirection.North && ship.length > coordinates.row) {
        ship.length - 1
      } else if (direction == BoardDirection.South && ship.length > (10 - coordinates.row)) {
        10 - ship.length
      } else {
        coordinates.row
      }
    }

    val startCol = {
      if (direction == BoardDirection.West && ship.length > coordinates.col) {
        ship.length - 1
      } else if (direction == BoardDirection.East && ship.length > (10 - coordinates.col)) {
        10 - ship.length
      } else {
        coordinates.col
      }
    }

    generateCoordinates(Coordinates(startRow, startCol), ship.length, direction) match {
      case Success(value) => placeSingleShip(ship, value)
      case Failure(ex) => Failure(ex)
    }
  }

  /** Places a single ship on board
   *
   * @param ship            ship to place
   * @param shipCoordinates coordinates of the ship
   * @return updated board
   */
  def placeSingleShip(ship: Ship, shipCoordinates: Vector[Coordinates]): Try[Board] = {
    // TODO
    // Fehlermonade, wenn Schiff hier nicht plaziert werden kann
    // Fehlermonade, wenn Schiff bereits platziert wurde
    // Fehlermonade, wenn Schiff nicht Teil des Bretts ist
    // Fehlermonade, wenn row / col Wert nicht möglich (< 0, > 9)

    val newShipPosition = ShipPosition(ship, shipCoordinates)
    Success(copy(matrix, ships, shipPositions :+ newShipPosition))
  }

  /** Shoot at BoardCell
   *
   * @param row row coordinate
   * @param col col coordinate
   * @return a tuple containing the updated board together with a boolean reflecting if the shot hit a ship
   */
  def shoot(row: Int, col: Int): (Board, Boolean) = {
    // TODO
    // Wichtig: Was passiert, wenn Board Cell bereits beschossen wurde?! --> MONADE!

    val newMatrix = matrix.updated(row, matrix(row).updated(col, BoardCell(true)))
    val shipPos = shipPositions.find(_.positions.contains(Coordinates(row, col)))
    (copy(newMatrix, ships, shipPositions), shipPos.isDefined)
  }

  /** Checks if ship is destroyed
   *
   * @param ship Ship to check
   * @return destroyed?
   */
  def isDestroyed(ship: Ship): Try[Boolean] = {
    val shipPosition = shipPositions.find(_.ship == ship)

    shipPosition
      .map(sp => Success(sp.positions.forall(c => {
        matrix(c.row)(c.col).isHit
      })))
      .getOrElse(Failure(new NoSuchElementException))
  }

  def areAllShipsDestroyed(): Try[Boolean] = {
    Try(ships.map(isDestroyed(_)).map {
      case Success(y) => y
      case Failure(ex) => throw ex
    }.reduce((res, cur) => res && cur))
  }
}
