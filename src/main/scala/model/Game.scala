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

    /** Check if all Ships of one Player are destroyed
     *
     * @return Game still running?
     */
    def isRunning(): Boolean = {
      !player1Board.ships.forall(player1Board.isDestroyed(_)) &&
        !player2Board.ships.forall(player2Board.isDestroyed(_))
    }

    /** Check if human player is winner
     *
     * @return Winner?
     */
    def humanPlayerIsWinner(): Boolean = {
      !player1Board.ships.forall(player1Board.isDestroyed(_)) &&
        player2Board.ships.forall(player2Board.isDestroyed(_))
    }
}