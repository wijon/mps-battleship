package model

import scala.util.{Failure, Success, Try}

/** Game-class manages boards and game specific values
 *
 * @param humanPlayerBoard Board of human player
 * @param aiPlayerBoard Board of AI player
 * @param roundNum     Current round number
 */
case class Game(humanPlayerBoard: Board, aiPlayerBoard: Board, roundNum: Int) {
  // Override constructor, this one is used for the initial instantiation
  def this(player1Ships: Vector[Ship], player2Ships: Vector[Ship]) =
    this(new Board(player1Ships), new Board(player2Ships), 0)

  /** Check if all Ships of one Player are destroyed
   *
   * @return Game still running?
   */
  def isRunning: Try[Boolean] = {
    val p1Result = humanPlayerBoard.areAllShipsDestroyed()
    val p2Result = aiPlayerBoard.areAllShipsDestroyed()

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
    isRunning match {
      case Success(value) => if (!value) aiPlayerBoard.areAllShipsDestroyed() else Success(false)
      case Failure(ex) => Failure(ex)
    }
  }
}