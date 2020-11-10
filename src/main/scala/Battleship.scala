import model.{Game, Ship}

import scala.util.{Failure, Success}

object Battleship {
  def main(args: Array[String]): Unit = {
    val game = new Game(getShips(), getShips())

    while (game.isRunning.isSuccess && game.isRunning.get) {

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