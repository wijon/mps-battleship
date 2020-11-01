package model

/** Game-class manages boards and game specific values
 *
 * @param player1Board Board of human player
 * @param player2Board Board of AI player
 * @param roundNum     Current round number
 */
case class Game(player1Board: Board, player2Board: Board, roundNum: Int) {
  // Override constructor, this one is used for the initial instantiation
  def this(player1Ships: Vector[Ship], player2Ships: Vector[Ship]) =
    this(new Board(player1Ships), new Board(player2Ships), 0)
}