package model

import scala.collection.immutable.HashMap

case class Board(matrix: Vector[Vector[BoardCell]], ships: Vector[Ship], shipPositions: Vector[ShipPosition]) {
  // Override constructor, this one is used for the initial instantiation
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
   * @return a tuple containing the updated board together with a boolean reflecting if the shot hit a ship
   */
  def shoot(x: Int, y: Int): (Board, Boolean) = {
    // TODO
    // Wichtig: Was passiert, wenn Board Cell bereits beschossen wurde?!

    // Update board cell in matrix
    val newMatrix = matrix.updated(y, matrix(y).updated(x, BoardCell(true)))

    // Check if a ship was hit
    val shipPos = shipPositions.find(sp => sp.positions.contains(Coordinates(x, y)))

    var isHit = false
    if (!shipPos.isEmpty) {
      // Ship was hit
      isHit = true
    }

    // Return a copy of the current object with the updated matrix
    return (copy(newMatrix, ships, shipPositions), isHit)
  }

  /** Checks if ship is destroyed
   *
   * @param ship Ship to check
   * @return destroyed?
   */
  def isDestroyed(ship: Ship): Boolean = {
    // Get position for given ship
    val shipPosition = shipPositions.find(sp => sp.ship == ship)

    // Check if there is a position given for the ship
    if (shipPosition.isEmpty) {
      // No position given, return false (ToDo: Handle differently?)
      return false
    }

    // Check if every board cell within the ships position is hit
    return shipPosition.get.positions.forall(c => {
      matrix(c.y)(c.x).isHit
    })
  }
}
