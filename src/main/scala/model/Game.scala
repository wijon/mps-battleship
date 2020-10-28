package model

object Game {
  /** Board of human player */
  var player1Board: Board = new Board(getShips())
  /** Board of AI player */
  var player2Board: Board = new Board(getShips())

  /** Check if all Ships of one Player are destroyed
   *
   * @return game still running?
   */
  def isRunning(): Boolean = {
    // TODO
    throw new NotImplementedError()
  }

  /** Check if human player is winner
   *
   * @return Winner?
   */
  def isWinner(): Boolean = {
    // TODO
    // Was passiert, wenn Spiel noch läuft?!
    throw new NotImplementedError()
  }

  /** Create ships
   *
   * @return List of all ships
   */
  def getShips(): Vector[Ship] = {
    // TODO
    // 5 Schiffe: 1 2er, 2 3er, 1 4er, 1 5er
    throw new NotImplementedError()
  }

  /** Make a move for board
   *
   * @param player1     board of player1? (human player)
   * @param coordinates coordinates to shoot
   * @return Board after play
   */
  def shootAtBoard(player1: Boolean, coordinates: Coordinates): Board = {
    // TODO
    throw new NotImplementedError()
  }
}