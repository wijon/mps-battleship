import model.{Board, BoardCell, Coordinates, Game, Ship, ShipPosition}
import org.scalatest.wordspec.AnyWordSpec

class BattleshipSpec extends AnyWordSpec {
  "Battleship" when {
    "getShips() is called" should {
      val ships = Battleship.getShips
      "return a total of 5 ships" in {
        assert(ships.length == 5)
      }
      "contain a Carrier with length 5" in {
        assert(ships.exists(s => s.name == "Carrier" && s.length == 5))
      }
      "contain a Battleship with length 4" in {
        assert(ships.exists(s => s.name == "Battleship" && s.length == 4))
      }
      "contain a Cruiser with length 3" in {
        assert(ships.exists(s => s.name == "Cruiser" && s.length == 3))
      }
      "contain a Submarine with length 3" in {
        assert(ships.exists(s => s.name == "Submarine" && s.length == 3))
      }
      "contain a Destroyer with length 2" in {
        assert(ships.exists(s => s.name == "Destroyer" && s.length == 2))
      }
    }
  }

  "Ships" when {
    "generated" should {
      val ships = Battleship.getShips

      "have different names" in {
        assert(ships.map(_.name).distinct.length == ships.length)
      }
    }
  }

  "Round text" when {
    "generated" should {
      val shipsHuman = Vector(
        Ship(2, "humanShip1")
      )
      val shipsAi = Vector(
        Ship(2, "aiShip1")
      )

      val shipPositionsHuman = Vector(
        ShipPosition(shipsHuman(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val shipPositionsAi = Vector(
        ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

      val testBoardHuman = Board(matrixHuman, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)
      val roundText = Battleship.generateRoundText(testGame)

      "not be empty" in {
        assert(roundText.nonEmpty)
      }
    }
  }

  "A game" when {
    val p1Ships = Vector(
      Ship(1, "A")
    )
    val p2Ships = Vector(
      Ship(2, "B")
    )
    val game = new Game(p1Ships, p2Ships)

    "starting a new round" should {
      val gameNextRound = Battleship.startNewRound(game)

      "have a round number which is 1 greater than last round" in {
        assert((game.roundNum + 1) == gameNextRound.roundNum)
      }
    }
  }
}
