import model.OutputHelper.{generateAiPlayerRoundInfoText, generateHumanPlayerRoundInfoText, generateNothingHitInfoText, generateShipHitInfoText, generateShootAgainInfoText, generateShootInfoText}
import model.{Game, Ship}

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Battleship {
  def main(args: Array[String]): Unit = {
    // New game. Randomly place ships
    new Game(getShips(), getShips()).placeAllShipsRandomly((maxValue: Int) => scala.util.Random.nextInt(maxValue)) match {
      case Success(game) => {
        // Play game. Round by Round
        play(printValue => println(printValue),
          startNewRound(game),
          () => StdIn.readLine(),
          () => {
            val input = scala.util.Random.nextInt(100).toString
            if (input.length == 1) "0" + input else input
          }) match {
          case Failure(ex) => throw ex
          case Success(value) => {
            // End of game. Show victory / loss
            model.OutputHelper.generateFinalText(value) match {
              case Success(value) => value.foreach(println(_))
              case Failure(ex) => throw ex
            }
          }
        }
      }
      case Failure(ex) => throw ex
    }
  }

  /** Play round after round until finish
   *
   * @param fktForInfoTextOutput            Function to put output text
   * @param game                            Game to play
   * @param fktHumanGetCoordinatesToShootAt Function to get coordinates to shoot at for human player
   * @param fktAiGetCoordinatesToShootAt    Function to get coordinates to shoot at for ai player
   * @return Finished game
   */
  def play(fktForInfoTextOutput: String => Unit,
           game: Game,
           fktHumanGetCoordinatesToShootAt: () => String,
           fktAiGetCoordinatesToShootAt: () => String
          ): Try[Game] = {
    generateRoundText(game).foreach(println(_))

    // Human
    (Vector("") ++ generateHumanPlayerRoundInfoText()).foreach(println(_))

    Try(playOneRoundOfOnePlayer(
      humanPlayerTurn = true,
      game,
      fktHumanGetCoordinatesToShootAt,
      fktForInfoTextOutput) match {
      case Failure(ex) => throw ex
      case Success(gameAfterHumanRound) => {

        // AI or finished?
        if (!gameAfterHumanRound.isRunning.isSuccess || !gameAfterHumanRound.isRunning.get) {
          gameAfterHumanRound
        } else {
          (Vector("") ++ generateAiPlayerRoundInfoText()).foreach(println(_))

          playOneRoundOfOnePlayer(
            humanPlayerTurn = false,
            gameAfterHumanRound,
            fktAiGetCoordinatesToShootAt,
            fktForInfoTextOutput) match {
            case Failure(ex) => throw ex
            case Success(gameAfterAiRound) => {
              Thread.sleep(1000)

              // Next round or finished?
              if (gameAfterAiRound.isRunning.isSuccess && gameAfterAiRound.isRunning.get) {
                play(fktForInfoTextOutput,
                  startNewRound(gameAfterAiRound),
                  fktHumanGetCoordinatesToShootAt,
                  fktAiGetCoordinatesToShootAt)
                match {
                  case Failure(ex) => throw ex
                  case Success(value) => value
                }
              } else {
                gameAfterAiRound
              }
            }
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
   * @return Game
   */
  def playOneRoundOfOnePlayer(humanPlayerTurn: Boolean,
                              game: Game,
                              fktGetCoordinatesToShootAt: () => String,
                              fktForInfoTextOutput: String => Unit
                             ): Try[Game] = {
    if (!humanPlayerTurn) Thread.sleep(1000)

    val input = fktGetCoordinatesToShootAt()
    val rowToShootAt = input.slice(0, 1).toInt
    val colToShootAt = input.slice(1, 2).toInt

    generateShootInfoText(rowToShootAt, colToShootAt).foreach(fktForInfoTextOutput(_))

    Try(game.shootAtBoard(!humanPlayerTurn, rowToShootAt, colToShootAt) match {
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

          playOneRoundOfOnePlayer(humanPlayerTurn, value._1, fktGetCoordinatesToShootAt, fktForInfoTextOutput) match {
            case Success(value) => value
            case Failure(ex) => throw ex
          }
        } else {
          value._1
        }
      case Failure(ex) => throw ex
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
    val humanBoard = model.OutputHelper.generateBoard(game.humanPlayerBoard, true, "Mensch")
    val aiBoard = model.OutputHelper.generateBoard(game.aiPlayerBoard, true, "KI")

    Vector(" ") ++ roundInfoText ++ Vector(" ") ++ shipsInfoText ++ Vector(" ") ++ humanBoard ++ Vector(" ") ++ aiBoard
  }

  /** Create ships
   *
   * @return List of all ships
   */
  def getShips(): Vector[Ship] = {
    Vector(
      Ship(5, "Carrier"),
      Ship(4, "Battleship"),
      Ship(3, "Cruiser"),
      Ship(3, "Submarine"),
      Ship(2, "Destroyer")
    )
  }
}