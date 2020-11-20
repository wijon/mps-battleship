package model

import dataTransferObjects.{Ship, ShipPosition}
import dsl.intern.ShipPlacement
import enums.BoardDirection

import scala.util.{Failure, Success, Try}

/** Game-class manages boards and game specific values
 *
 * @param humanPlayerBoard Board of human player
 * @param aiPlayerBoard    Board of AI player
 * @param roundNum         Current round number
 */
case class Game(humanPlayerBoard: Board, aiPlayerBoard: Board, roundNum: Int) {
  /** Override constructor, this one is used for the initial instantiation
   */
  def this(player1Ships: Vector[Ship], player2Ships: Vector[Ship]) =
    this(new Board(player1Ships), new Board(player2Ships), 0)

  /** Start new round of battleship. Increment round number
   *
   * @return Changed game
   */
  def startNewRound(): Game = {
    copy(humanPlayerBoard, aiPlayerBoard, roundNum + 1)
  }

  /** DSL function for manually placing ships
   *
   * @param init A function which handles the ship placement
   * @return
   */
  def ships(init: ShipPlacement => Unit): Game = {
    val placement = new ShipPlacement
    init(placement)

    val board1 = Board(placement.shipPositionsPlayer1.toVector)
    val board2 = Board(placement.shipPositionsPlayer2.toVector)

    // ToDo: Question for Marko
    // Instead of returning copy, should we change 'humanPlayerBoard' and 'aiPlayerBoard' to 'var'
    // and assign 'board1' and 'board2' to it? What's the correct "DSL" way to do it?
    //    humanPlayerBoard = board1
    //    aiPlayerBoard = board2
    //    this
    copy(board1, board2, roundNum)
  }

  /** Places all ships of both board. Uses parameter to generate coordinates
   *
   * @param randomIntGenerator Function for random ints
   * @return Modified game
   */
  def placeAllShipsRandomly(randomIntGenerator: Int => Int): Try[Game] = {
    val humanBoard = placeAllShipsOfBoardRandomlyOneByOne(humanPlayerBoard, humanPlayerBoard.ships, randomIntGenerator)
    val aiBoard = placeAllShipsOfBoardRandomlyOneByOne(aiPlayerBoard, aiPlayerBoard.ships, randomIntGenerator)

    if (humanBoard.isFailure) {
      Failure(humanBoard.failed.get)
    } else if (aiBoard.isFailure) {
      Failure(aiBoard.failed.get)
    } else {
      Success(copy(humanBoard.get, aiBoard.get, roundNum))
    }
  }

  /** Place ships of board recursively
   *
   * @param board              Board
   * @param remainingShips     Ships to place
   * @param randomIntGenerator Function for coordinate and BoardDirection generation
   * @return Board with ships placed
   */
  private def placeAllShipsOfBoardRandomlyOneByOne(board: Board,
                                                   remainingShips: Vector[Ship],
                                                   randomIntGenerator: Int => Int): Try[Board] = {
    if (remainingShips.isEmpty) {
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

  /** Shoot at player board
   *
   * @param humanPlayerBoard Shoot at human player board? (false = shoot at ai player board)
   * @param row              Row to shoot at
   * @param col              Column to shoot at
   * @return Game and is ship hit?
   */
  def shootAtBoard(humanPlayerBoard: Boolean, row: Int, col: Int): Try[(Game, Option[ShipPosition])] = {
    if (humanPlayerBoard) shootAtHumanBoard(row, col) else shootAtAiBoard(row, col)
  }

  /** Shoot at human player board
   *
   * @param row Row to shoot
   * @param col Column to shoot
   * @return Game and Is ship hit?
   */
  private def shootAtHumanBoard(row: Int, col: Int): Try[(Game, Option[ShipPosition])] = {
    Try(humanPlayerBoard.shoot(row, col) match {
      case Success(value) => (copy(value.board, aiPlayerBoard, roundNum), value.shipPosition)
      case Failure(ex) => throw ex
    })
  }

  /** Shoot at ai player board
   *
   * @param row Row to shoot
   * @param col Column to shoot
   * @return Game and Is ship hit?
   */
  private def shootAtAiBoard(row: Int, col: Int): Try[(Game, Option[ShipPosition])] = {
    Try(aiPlayerBoard.shoot(row, col) match {
      case Success(value) => (copy(humanPlayerBoard, value.board, roundNum), value.shipPosition)
      case Failure(ex) => throw ex
    })
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
  def humanPlayerIsWinner(): Boolean = {
    val aiPlayerAllShipsDestroyed = aiPlayerBoard.areAllShipsDestroyed()
    !(aiPlayerAllShipsDestroyed.isFailure || !aiPlayerAllShipsDestroyed.get)
  }
}

object Game {
  def newGame(init: Game => Game): Game = {
    init(new Game(Vector.empty, Vector.empty))
  }
}