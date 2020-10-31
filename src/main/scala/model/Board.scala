package model

import scala.collection.immutable.HashMap

case class Board(matrix: Vector[Vector[BoardCell]], ships: Vector[Ship], shipPositions: Vector[(Ship, Coordinates)]) {
  def this(ships: Vector[Ship]) = this(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, ships, Vector())

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
  def shoot(x: Int, y: Int): (Board, Boolean) = {
    // TODO
    // Wichtig: Was passiert, wenn Board Cell bereits beschossen wurde?!

    // Update board cell in matrix
    val newMatrix = matrix.updated(y, matrix(y).updated(x, BoardCell(true)))

    // Check if ship was hit
    val shipPos = shipPositions.find(v => v._2 == Coordinates(x,y))

    var isHit = false
    if(!shipPos.isEmpty){
      // Ship was hit
      isHit = true
    }

    return (copy(newMatrix, ships, shipPositions), isHit)
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
