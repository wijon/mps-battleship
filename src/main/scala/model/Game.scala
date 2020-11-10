package model

import scala.util.{Failure, Success, Try}

/** Game-class manages boards and game specific values
 *
 * @param humanPlayerBoard Board of human player
 * @param aiPlayerBoard    Board of AI player
 * @param roundNum         Current round number
 */
case class Game(humanPlayerBoard: Board, aiPlayerBoard: Board, roundNum: Int) {
  // Override constructor, this one is used for the initial instantiation
  def this(player1Ships: Vector[Ship], player2Ships: Vector[Ship]) =
    this(new Board(player1Ships), new Board(player2Ships), 0)

  /** Places all ships of both board. Uses parameter to generate coordinates
   *
   * @param randomIntGenerator Function for random ints
   * @return Modified game
   */
  def placeAllShipsRandomly(randomIntGenerator: Int => Int): Try[Game] = {
    val humanBoard = placeAllShipsOfBoardRandomlyOneByOne(humanPlayerBoard, humanPlayerBoard.ships, randomIntGenerator)
    val aiBoard = placeAllShipsOfBoardRandomlyOneByOne(aiPlayerBoard, aiPlayerBoard.ships, randomIntGenerator)

    if(humanBoard.isFailure) {
      Failure(humanBoard.failed.get)
    } else if(aiBoard.isFailure) {
      Failure(aiBoard.failed.get)
    } else {
      Success(copy(humanBoard.get, aiBoard.get, roundNum))
    }
  }

  /** Place ships of board recursively
   *
   * @param board Board
   * @param remainingShips Ships to place
   * @param randomIntGenerator Function for coordinate and BoardDirection generation
   * @return Board with ships placed
   */
  def placeAllShipsOfBoardRandomlyOneByOne(board: Board,
                                           remainingShips: Vector[Ship],
                                           randomIntGenerator: Int => Int): Try[Board] = {
    if (remainingShips.length == 0) {
      Success(board)
    } else {
      placeAllShipsOfBoardRandomlyOneByOne(board, remainingShips.drop(1), randomIntGenerator) match {
        case Success(board) => board.placeSingleShipForce(remainingShips(0), randomIntGenerator, randomIntGenerator,
          (maxValue: Int) => {
            BoardDirection(randomIntGenerator(maxValue))
          }, 100)
        case Failure(ex) => Failure(ex)
      }
    }
  }

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