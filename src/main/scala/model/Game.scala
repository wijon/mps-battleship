package model

import dataTransferObjects.functionResults.GameShotAtResult
import dataTransferObjects.{Ship}
import dsl.intern.ShipPlacement

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
  def this(player1Ships: Vector[Ship], player2Ships: Vector[Ship], fktRandomInt: Int => Int) =
    this(Board(player1Ships, fktRandomInt), Board(player2Ships, fktRandomInt), 0)

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

  /** Shoot at player board
   *
   * @param humanPlayerBoard Shoot at human player board? (false = shoot at ai player board)
   * @param row              Row to shoot at
   * @param col              Column to shoot at
   * @return Game and is ship hit?
   */
  def shootAtBoard(humanPlayerBoard: Boolean, row: Int, col: Int): Try[GameShotAtResult] = {
    if (humanPlayerBoard) shootAtHumanBoard(row, col) else shootAtAiBoard(row, col)
  }

  /** Shoot at human player board
   *
   * @param row Row to shoot
   * @param col Column to shoot
   * @return Game and Is ship hit?
   */
  private def shootAtHumanBoard(row: Int, col: Int): Try[GameShotAtResult] = {
    Try(humanPlayerBoard.shoot(row, col) match {
      case Success(value) => GameShotAtResult(copy(value.board, aiPlayerBoard, roundNum), value.shipPosition)
      case Failure(ex) => throw ex
    })
  }

  /** Shoot at ai player board
   *
   * @param row Row to shoot
   * @param col Column to shoot
   * @return Game and Is ship hit?
   */
  private def shootAtAiBoard(row: Int, col: Int): Try[GameShotAtResult] = {
    Try(aiPlayerBoard.shoot(row, col) match {
      case Success(value) => GameShotAtResult(copy(humanPlayerBoard, value.board, roundNum), value.shipPosition)
      case Failure(ex) => throw ex
    })
  }

  /** Check if all Ships of one Player are destroyed
   *
   * @return Game still running?
   */
  def isRunning: Boolean = {
    !humanPlayerBoard.areAllShipsDestroyed() && !aiPlayerBoard.areAllShipsDestroyed()
  }

  /** Check if human player is winner
   *
   * @return Winner?
   */
  def humanPlayerIsWinner(): Boolean = {
    aiPlayerBoard.areAllShipsDestroyed()
  }
}

object Game {
  def newGame(init: Game => Game): Game = {
    init(new Game(Vector.empty, Vector.empty, (_: Int) => 0))
  }
}