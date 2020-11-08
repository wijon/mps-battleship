package model

import scala.util.{Failure, Success, Try}

case class Board(matrix: Vector[Vector[BoardCell]], ships: Vector[Ship], shipPositions: Vector[ShipPosition]) {
  // Override constructor, this one is used for the initial instantiation
  def this(ships: Vector[Ship]) = this(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, ships, Vector())

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
