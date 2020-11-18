import model.OutputHelper._
import model.{Board, Coordinates, Game, Ship}

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Battleship {
  def main(args: Array[String]): Unit = {
    // New game. Randomly place ships
    new Game(getShips, getShips).placeAllShipsRandomly((maxValue: Int) => scala.util.Random.nextInt(maxValue)) match {
      case Success(game) =>
        // Play game. Round by Round
        play(printValue => println(printValue),
          startNewRound(game),
          () => StdIn.readLine(),
          () => {
            val input = scala.util.Random.nextInt(100).toString
            if (input.length == 1) "0" + input else input
          },
          1000) match {
          case Failure(ex) => throw ex
          case Success(value) =>
            // End of game. Show victory / loss
            model.OutputHelper.generateFinalText(value) match {
              case Success(value) => value.foreach(println(_))
              case Failure(ex) => throw ex
            }
        }
      case Failure(ex) => throw ex
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
        if (currentIteration == maxIterations)
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
      val row = input.slice(0, 1)
      val col = input.slice(1, 2)

      if (!row.charAt(0).isDigit) {
        generateInvalidRowInputInfoText().foreach(fktErrorMessageOutput(_))
        throw new IndexOutOfBoundsException(row)
      }

      if (!col.charAt(0).isDigit) {
        generateInvalidColInputInfoText().foreach(fktErrorMessageOutput(_))
        throw new IndexOutOfBoundsException(col)
      }

      val rowAsInt = row.toInt
      val colAsInt = col.toInt

      if (board.isHit(rowAsInt, colAsInt)) {
        generateInvalidInputInfoText().foreach(fktErrorMessageOutput(_))
        throw new IndexOutOfBoundsException(input)
      }

      Coordinates(rowAsInt, colAsInt)
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
    generateRoundText(game).foreach(fktForInfoTextOutput(_))

    // Human
    (Vector("") ++ generateHumanPlayerRoundInfoText()).foreach(fktForInfoTextOutput(_))

    Try(playOneRoundOfOnePlayer(humanPlayerTurn = true, game, fktHumanGetCoordinatesToShootAt,
      fktForInfoTextOutput, 0) match {
      case Failure(ex) => throw ex
      case Success(gameAfterHumanRound) =>

        // AI or finished?
        if (!gameAfterHumanRound.isRunning.isSuccess || !gameAfterHumanRound.isRunning.get) {
          gameAfterHumanRound
        } else {
          (Vector("") ++ generateAiPlayerRoundInfoText()).foreach(fktForInfoTextOutput(_))

          playOneRoundOfOnePlayer(humanPlayerTurn = false, gameAfterHumanRound, fktAiGetCoordinatesToShootAt,
            fktForInfoTextOutput, aiPlayerDelay) match {
            case Failure(ex) => throw ex
            case Success(gameAfterAiRound) =>
              Thread.sleep(aiPlayerDelay)

              // Next round or finished?
              if (gameAfterAiRound.isRunning.isSuccess && gameAfterAiRound.isRunning.get) {
                play(fktForInfoTextOutput, startNewRound(gameAfterAiRound), fktHumanGetCoordinatesToShootAt,
                  fktAiGetCoordinatesToShootAt, aiPlayerDelay)
                match {
                  case Failure(ex) => throw ex
                  case Success(value) => value
                }
              } else {
                gameAfterAiRound
              }
          }
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

    Try(waitingForCorrectInput(currentIteration = 0, maxIterations = 10, fktGetInput = fktGetCoordinatesToShootAt,
      board = if (humanPlayerTurn) game.aiPlayerBoard else game.humanPlayerBoard,
      fktErrorMessageOutput = fktForInfoTextOutput) match {
      case Failure(ex) => throw ex
      case Success(value) =>
        generateShootInfoText(value.row, value.col).foreach(fktForInfoTextOutput(_))

        game.shootAtBoard(!humanPlayerTurn, value.row, value.col) match {
          case Success(value) =>
            if (value._2.isDefined) {
              if (humanPlayerTurn)
                generateShipHitInfoText(value._2.get.ship, value._1.aiPlayerBoard.isDestroyed(value._2.get.ship).get)
                  .foreach(fktForInfoTextOutput(_))
              else
                generateShipHitInfoText(value._2.get.ship, value._1.humanPlayerBoard.isDestroyed(value._2.get.ship).get)
                  .foreach(fktForInfoTextOutput(_))
            } else {
              generateNothingHitInfoText().foreach(fktForInfoTextOutput(_))
            }

            if (value._2.isDefined && value._1.isRunning.isSuccess && value._1.isRunning.get) {
              generateShootAgainInfoText().foreach(fktForInfoTextOutput(_))

              playOneRoundOfOnePlayer(humanPlayerTurn, value._1, fktGetCoordinatesToShootAt, fktForInfoTextOutput,
                delay) match {
                case Success(value) => value
                case Failure(ex) => throw ex
              }
            } else {
              value._1
            }
          // Failure not possible --> waitingForCorrectInput() checks, if cell was already hit
          //          case Failure(ex) => throw ex
        }
    })
  }

  /** Start new round of battleship. Increment round number
   *
   * @param game Game
   * @return Changed game
   */
  def startNewRound(game: Game): Game = {
    game.copy(game.humanPlayerBoard, game.aiPlayerBoard, game.roundNum + 1)
  }

  /** Generate round text for output
   *
   * @param game Game
   * @return Round text for output
   */
  def generateRoundText(game: Game): Vector[String] = {
    val roundInfoText = model.OutputHelper.generateRoundInfoText(game)
    val shipsInfoText = model.OutputHelper.generateRemainingShips(game.humanPlayerBoard, "Mensch")
    val humanBoard = model.OutputHelper.generateBoard(game.humanPlayerBoard, showShips = true, "Mensch")
    val aiBoard = model.OutputHelper.generateBoard(game.aiPlayerBoard, showShips = true, "KI")

    val maxLength = humanBoard.map(str => str.length).max
    val boardText = (humanBoard zip aiBoard).map(x => s"${x._1.padTo(maxLength, ' ')}\t\t\t${x._2}")

    Vector(" ") ++ roundInfoText ++ Vector(" ") ++ shipsInfoText ++ Vector(" ") ++ boardText
  }

  /** Create ships
   *
   * @return List of all ships
   */
  def getShips: Vector[Ship] = {
    Vector(
      Ship(5, "Carrier"),
      Ship(4, "Battleship"),
      Ship(3, "Cruiser"),
      Ship(3, "Submarine"),
      Ship(2, "Destroyer")
    )
  }
}