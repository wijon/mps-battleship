package model

import scala.util.{Failure, Success, Try}

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
  def isRunning(): Try[Boolean] = {
    val p1Result = player1Board.areAllShipsDestroyed()
    val p2Result = player2Board.areAllShipsDestroyed()

    if (p1Result.isFailure) {
      p1Result
    } else if (p2Result.isFailure) {
      p2Result
    } else {
      Success(!p1Result.get && !p2Result.get)
    }
  }

  /** Check if human player is winner
   *
   * @return Winner?
   */
  def humanPlayerIsWinner(): Try[Boolean] = {
    isRunning() match {
      case Success(value) => if (value) player2Board.areAllShipsDestroyed() else Success(false)
      case Failure(ex) => Failure(ex)
    }
  }
}