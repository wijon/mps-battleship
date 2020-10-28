package model

import scala.collection.immutable.HashMap

case class Board(ships: Vector[Ship]) {
  val matrix: HashMap[Coordinates, BoardCell] = placeShips()

  /** Places all ships in matrix
   *
   * @return matrix with ships
   */
  def placeShips(): HashMap[Coordinates, BoardCell] = {
    // TODO
    // Wichtig: Random Generator als Übergabeparameter verwenden!
    throw new NotImplementedError()
  }

  /** Shoot at BoardCell
   *
   * @param x X coordinate
   * @param y Y coordinate
   * @return hit BoardCell
   */
  def shoot(x: Int, y: Int): BoardCell = {
    // TODO
    // Wichtig: Was passiert, wenn Board Cell bereits beschossen wurde?!
    throw new NotImplementedError()
  }

  /** Checks if ship is destroyed
   *
   * @param ship Ship to check
   * @return destroyed?
   */
  def isDestroyed(ship: Ship): Boolean = {
    // TODO
    throw new NotImplementedError()
  }
}
