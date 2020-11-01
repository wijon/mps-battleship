package model

object Game {
  /** Board of human player */
  var player1Board: Board = new Board(getShips())
  /** Board of AI player */
  var player2Board: Board = new Board(getShips())

  /** Create ships
   *
   * @return List of all ships
   */
  def getShips(): Vector[Ship] = {
    return Vector(
      Ship(5, "Carrier"),
      Ship(4, "Battleship"),
      Ship(3, "Cruiser"),
      Ship(3, "Submarine"),
      Ship(2, "Destroyer")
    )
  }
}