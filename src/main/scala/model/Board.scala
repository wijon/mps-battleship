package model

import java.security.InvalidParameterException

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
                          movingDirection: BoardDirection): Vector[Coordinates] = {
    if (remainingMoves <= 0) {
      Vector.empty
    } else {
      generateCoordinates(moveOneField(currentCoordinates, movingDirection), remainingMoves - 1, movingDirection) :+
        currentCoordinates
    }
  }

  /** moves one field from coordinates into movingDirection and gives new coordinates
   *
   * @param coordinates     coordinates from where to move
   * @param movingDirection moving direction
   * @return new coordinates
   */
  private def moveOneField(coordinates: Coordinates, movingDirection: BoardDirection): Coordinates = {
    movingDirection match {
      case BoardDirection.North => Coordinates(coordinates.row - 1, coordinates.col)
      case BoardDirection.East => Coordinates(coordinates.row, coordinates.col + 1)
      case BoardDirection.South => Coordinates(coordinates.row + 1, coordinates.col)
      case BoardDirection.West => Coordinates(coordinates.row, coordinates.col - 1)
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

    placeSingleShip(ship, generateCoordinates(Coordinates(startRow, startCol), ship.length, direction))
  }

  /** Places a single ship on board
   *
   * @param ship            ship to place
   * @param shipCoordinates coordinates of the ship
   * @return updated board
   */
  def placeSingleShip(ship: Ship, shipCoordinates: Vector[Coordinates]): Try[Board] = {
    if (!shipBelongsToBoard(ship) ||
      shipIsPlacedOnBoard(ship) ||
      !noShipIsPlacedAtCoordinates(shipCoordinates) ||
      !coordinatesAreCorrect(shipCoordinates)) {
      Failure(new InvalidParameterException)
    } else {
      Success(copy(matrix, ships, shipPositions :+ ShipPosition(ship, shipCoordinates)))
    }
  }

  /** Check if ship belongs to board
   *
   * @param ship Ship to check
   * @return Ship belongs to board?
   */
  private def shipBelongsToBoard(ship: Ship): Boolean = {
    ships.contains(ship)
  }

  /** Check if ship is already placed on board
   *
   * @param ship Ship to check
   * @return Ship is already placed on board?
   */
  private def shipIsPlacedOnBoard(ship: Ship): Boolean = {
    shipPositions.exists(_.ship == ship)
  }

  /** Check if coordinates aren't occupied yet
   *
   * @param coordinates Coordinates to check
   * @return Coordinates aren't occupied?
   */
  private def noShipIsPlacedAtCoordinates(coordinates: Vector[Coordinates]): Boolean = {
    coordinates.forall(c => !shipPositions.exists(_.positions.contains(c)))
  }

  /** Check if coordinates are correct (0 <= row / col <= 9)
   *
   * @param coordinates Coordinates to check
   * @return Coordinates are correct?
   */
  private def coordinatesAreCorrect(coordinates: Vector[Coordinates]): Boolean = {
    coordinates.forall(c => (0 to 9 contains c.row) && (0 to 9 contains c.col))
  }

  /** Shoot at BoardCell
   *
   * @param row row coordinate
   * @param col col coordinate
   * @return a tuple containing the updated board together with a boolean reflecting if the shot hit a ship
   */
  def shoot(row: Int, col: Int): Try[ShotAtResult] = {
    if (matrix(row)(col).isHit) {
      Failure(new UnsupportedOperationException)
    } else {
      val newMatrix = matrix.updated(row, matrix(row).updated(col, BoardCell(true)))
      val shipPos = shipPositions.find(_.positions.contains(Coordinates(row, col)))

      Success(ShotAtResult(copy(newMatrix, ships, shipPositions), shipPos.isDefined))
    }
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

  /** Checks if all ships of this board are destroyed
   *
   * @return true if all are destroyed, otherwise false
   */
  def areAllShipsDestroyed(): Try[Boolean] = {
    Try(ships.map(isDestroyed).map {
      case Success(y) => y
      case Failure(ex) => throw ex
    }.reduce((res, cur) => res && cur))
  }
}
