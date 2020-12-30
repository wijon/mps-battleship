import dataTransferObjects.Coordinates
import dsl.extern.FleetParser
import view.OutputHelper._
import model.{Board, Game}
import view.OutputHelper

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

/*
  Scala 3 Changes:
  - Enums
  - New if-Notation
  - Remove "new" keyword
    --> Couldn't be removed every time. We use multiple constructors. Using "new" / not using "new" defines, which constructor is used.
  - New for-Notation

  Unsuccessful:
  - Main-function as toplevel
    --> When using parameters in main-function, sbt run doesn't work anymore. Main-function without parameters works fine.
*/

object Battleship {
  def main(args: Array[String]): Unit = {
    val game = 
    if args.isEmpty then
      // New game. Randomly place ships
      new Game(Settings.getShipsForOnePlayer, Settings.getShipsForOnePlayer,
        (maxValue: Int) => scala.util.Random.nextInt(maxValue))
    else
      // New game get ship + positions from file
      createGameWithExplicitShips(args(0))

    // Play game. Round by Round
    play(printValue => println(printValue),
      game.startNewRound(),
      () => StdIn.readLine(),
      () => {
        val input = scala.util.Random.nextInt(100).toString
        if input.length == 1 then "0" + input else input
      },
      1000) match {
      case Failure(ex) => throw ex
      case Success(value) =>
        // End of game. Show victory / loss
        OutputHelper.generateFinalText(value) match {
          case Success(value) => value.foreach(println(_))
          case Failure(ex) => throw ex
        }
    }
  }

  /** Create a game utilizing the external and internal ship placement DSL by receiving a string containing
   * the player fleets formatted to match the external DSL format
   *
   * @param txt player fleets formatted to match the external DSL format
   * @return An initialized game object
   */
  def createGameWithExplicitShips(txt: String): Game = {
    val parser = new FleetParser
    parser.parseFleetText(txt) match {
      case Failure(ex) => throw ex
      case Success(value) =>
        Game.newGame { game =>
          game ships { ship =>
            value.map(fpr => fpr.ships.map(s => (s, fpr.player))).flatten(sp => sp).map(sp => {
              ship place sp._1.name length sp._1.length at sp._1.startingPos facing sp._1.dir as sp._2
            })
          }
        }
    }
  }

  /** Trying to get correct input for [maxIterations] times
   *
   * @param currentIteration      Current number of try
   * @param maxIterations         Maximum number of tries before failure
   * @param fktGetInput           Function for getting board coordinates
   * @param board                 Board to shoot at
   * @param fktErrorMessageOutput Function to put error message
   * @return Input-coordinates
   */
  @tailrec
  def waitingForCorrectInput(currentIteration: Int,
                             maxIterations: Int,
                             fktGetInput: () => String,
                             board: Board,
                             fktErrorMessageOutput: String => Unit): Try[Coordinates] = {
    val input = fktGetInput()

    checkInputForCorrectCoordinates(input, board, fktErrorMessageOutput) match {
      case Success(value) => Success(value)
      case Failure(ex) =>
        if currentIteration == maxIterations then
          Failure(ex)
        else
          waitingForCorrectInput(currentIteration + 1, maxIterations, fktGetInput, board, fktErrorMessageOutput)
    }
  }

  /** Try to extract coordinates out of input-string
   *
   * @param input                 Input string
   * @param board                 Board to shoot at
   * @param fktErrorMessageOutput Function to put error message
   * @return Coordinates
   */
  def checkInputForCorrectCoordinates(input: String,
                                      board: Board,
                                      fktErrorMessageOutput: String => Unit): Try[Coordinates] = {
    Try({
      val coordinates = Coordinates.coordinatesFromString(input)

      coordinates match {
        case Success(value) =>
          if board.isHit(value.row, value.col) then
            generateInvalidInputInfoText().foreach(fktErrorMessageOutput(_))
            throw IndexOutOfBoundsException(input)

          value
        case Failure(ex) =>
          if ex.getMessage == "row" then
            generateInvalidRowInputInfoText().foreach(fktErrorMessageOutput(_))
          else
            generateInvalidColInputInfoText().foreach(fktErrorMessageOutput(_))

          throw ex
      }
    })
  }

  /** Play round after round until finish
   *
   * @param fktForInfoTextOutput            Function to put output text
   * @param game                            Game to play
   * @param fktHumanGetCoordinatesToShootAt Function to get coordinates to shoot at for human player
   * @param fktAiGetCoordinatesToShootAt    Function to get coordinates to shoot at for ai player
   * @param aiPlayerDelay                   Delay in ms after every ai action
   * @return Finished game
   */
  def play(fktForInfoTextOutput: String => Unit,
           game: Game,
           fktHumanGetCoordinatesToShootAt: () => String,
           fktAiGetCoordinatesToShootAt: () => String,
           aiPlayerDelay: Int
          ): Try[Game] = {
    if !game.isRunning then
      return Success(game)

    generateRoundText(game).foreach(fktForInfoTextOutput(_))

    // Human
    (Vector("") ++ generateHumanPlayerRoundInfoText()).foreach(fktForInfoTextOutput(_))

    Try(playOneRoundOfOnePlayer(humanPlayerTurn = true, game, fktHumanGetCoordinatesToShootAt,
      fktForInfoTextOutput, 0) match {
      case Failure(ex) => throw ex
      case Success(gameAfterHumanRound) =>

        // AI or finished?
        if (!gameAfterHumanRound.isRunning) then
          gameAfterHumanRound
        else
          (Vector("") ++ generateAiPlayerRoundInfoText()).foreach(fktForInfoTextOutput(_))

          playOneRoundOfOnePlayer(humanPlayerTurn = false, gameAfterHumanRound, fktAiGetCoordinatesToShootAt,
            fktForInfoTextOutput, aiPlayerDelay) match {
            case Failure(ex) => throw ex
            case Success(gameAfterAiRound) =>
              Thread.sleep(aiPlayerDelay)

              // Next round
              play(fktForInfoTextOutput, gameAfterAiRound.startNewRound(), fktHumanGetCoordinatesToShootAt,
                fktAiGetCoordinatesToShootAt, aiPlayerDelay).get
          }
    })
  }

  /** Play until no ship was hit
   *
   * @param humanPlayerTurn            Is it human players turn?
   * @param game                       Game to play
   * @param fktGetCoordinatesToShootAt Function to get coordinates
   * @param fktForInfoTextOutput       Function to put output text
   * @param delay                      Delay in ms after every move
   * @return Game
   */
  private def playOneRoundOfOnePlayer(humanPlayerTurn: Boolean,
                                      game: Game,
                                      fktGetCoordinatesToShootAt: () => String,
                                      fktForInfoTextOutput: String => Unit,
                                      delay: Int
                                     ): Try[Game] = {
    Thread.sleep(delay)

    Try(waitingForCorrectInput(currentIteration = 0, maxIterations = 100, fktGetInput = fktGetCoordinatesToShootAt,
      board = if humanPlayerTurn then game.aiPlayerBoard else game.humanPlayerBoard,
      fktErrorMessageOutput = fktForInfoTextOutput) match {
      case Failure(ex) => throw ex
      case Success(value) =>
        generateShootInfoText(value.row, value.col).foreach(fktForInfoTextOutput(_))

        game.shootAtBoard(!humanPlayerTurn, value.row, value.col) match {
          case Success(value) =>
            if value.shipPosition.isDefined then
              if (humanPlayerTurn) then
                generateShipHitInfoText(value.shipPosition.get.ship,
                  value.game.aiPlayerBoard.isDestroyed(value.shipPosition.get.ship))
                  .foreach(fktForInfoTextOutput(_))
              else
                generateShipHitInfoText(value.shipPosition.get.ship,
                  value.game.humanPlayerBoard.isDestroyed(value.shipPosition.get.ship))
                  .foreach(fktForInfoTextOutput(_))
            else 
              generateNothingHitInfoText().foreach(fktForInfoTextOutput(_))
            

            if (value.shipPosition.isDefined && value.game.isRunning) then
              generateShootAgainInfoText().foreach(fktForInfoTextOutput(_))

              playOneRoundOfOnePlayer(humanPlayerTurn, value.game, fktGetCoordinatesToShootAt, fktForInfoTextOutput,
                delay).get
            else
              value.game
          // Failure impossible --> waitingForCorrectInput() checks, if cell was already hit
          //          case Failure(ex) => throw ex
        }
    })
  }
}