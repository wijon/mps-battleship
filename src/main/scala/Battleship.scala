import model.{Game, Ship}

import scala.util.{Failure, Success}

object Battleship {
  def main(args: Array[String]): Unit = {
    var game = new Game(getShips(), getShips())
    var humanTurn = true

    while (game.isRunning.isSuccess && game.isRunning.get) {
      game = startNewRound(game)
      generateRoundText(game).foreach(print(_))

      while(humanTurn && game.isRunning.isSuccess && game.isRunning.get) {
        humanTurn = !humanTurn
      }

      while(!humanTurn && game.isRunning.isSuccess && game.isRunning.get) {
        humanTurn = !humanTurn
      }
    }

    model.OutputHelper.generateFinalText(game) match {
      case Failure(ex) => throw ex
      case Success(value) => value.foreach(println(_))
    }


    // --> 1 Spieler, 1 KI
    // --> Boards instanziieren, Schiffe random verteilen

    //while (Game.isRunning()) {
    // Clear Console
    // RoundInfoText ausgeben
    // RemainingShips ausgeben
    // Spielfelder ausgeben (eigenes und gegnerisches)
    // Eingabe erwarten
    // Alle Texte neu ausgeben
    // --> Treffer: Spiel vorbei? game.running=false : Eingabe erwarten
    // --> Kein Treffer: KI spielt, Spiel vorbei? game.running=false : Alle Texte neu ausgeben
    //}

    // Spiel vorbei: Victory / Loss ausgeben
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
    val aiBoard = model.OutputHelper.generateBoard(game.aiPlayerBoard, false, "KI")

    roundInfoText ++ Vector(" ") ++ shipsInfoText ++ Vector(" ") ++ humanBoard ++ Vector(" ") ++ aiBoard
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