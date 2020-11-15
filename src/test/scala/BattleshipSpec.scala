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

    "check input for correct coordinates" should {
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixWithHit = matrix.updated(4, matrix(4).updated(2, BoardCell(true)))
      val boardWithHitAt42 = Board(matrixWithHit, Vector(), Vector())

      "succeed for correct input" in {
        val result = Battleship.checkInputForCorrectCoordinates("35", boardWithHitAt42, _ => {})

        assert(result.isSuccess)
        assert(result.get.row == 3)
        assert(result.get.col == 5)
      }

      "fail for non numeric input for row" in {
        val result = Battleship.checkInputForCorrectCoordinates("a5", boardWithHitAt42, _ => {})

        assert(result.isFailure)
      }

      "fail for non numeric input for col" in {
        val result = Battleship.checkInputForCorrectCoordinates("3$", boardWithHitAt42, _ => {})

        assert(result.isFailure)
      }

      "fail for already hit board cell" in {
        val result = Battleship.checkInputForCorrectCoordinates("42", boardWithHitAt42, _ => {})

        assert(result.isFailure)
      }
    }

    "waiting for correct input" should {
      val board = new Board(Vector(Ship(2, "Test")))

      "give coordinates for correct input" in {
        val result = Battleship.waitingForCorrectInput(0, 1, () => "12", board, _ => {})

        assert(result.isSuccess)
        assert(result.get.row == 1)
        assert(result.get.col == 2)
      }

      "fail for invalid input" in {
        // Runs twice with same input (currentIteration < maxIteration) this will cover both branches of the if
        // in the Failure case of waitingForCorrectInput()
        val result = Battleship.waitingForCorrectInput(0, 1, () => "a2", board, _ => {})

        assert(result.isFailure)
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

  "Playing the game" when {
    val singleShipHuman = Vector(
      Ship(1, "humanShip1")
    )

    val singleShipAi = Vector(
      Ship(1, "aiShip1")
    )
    val doubleShipAi = Vector(
      Ship(2, "aiShip1")
    )

    val singleShipPositionsHuman = Vector(
      ShipPosition(singleShipHuman(0), Vector(Coordinates(0, 0))),
    )

    val singleShipPositionsAi = Vector(
      ShipPosition(singleShipAi(0), Vector(Coordinates(0, 0))),
    )

    val doubleShipPositionsAi = Vector(
      ShipPosition(doubleShipAi(0), Vector(Coordinates(0, 0), Coordinates(0, 1))),
    )

    val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
    val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

    "human player is winning" should {
      val testBoardHuman = Board(matrixHuman, singleShipHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "00", () => "11")

      "show human as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().get)
      }
    }

    "ai player is winning" should {
      val testBoardHuman = Board(matrixHuman, singleShipHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "11", () => "00")

      "show ai as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().isSuccess)
        assert(!gameAfterPlaying.get.humanPlayerIsWinner().get)
      }
    }

    "max number of wrong inputs exceeds" should {
      val testBoardHuman = Board(matrixHuman, singleShipHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "11", () => "11")

      "fail" in {
        assert(gameAfterPlaying.isFailure)
      }
    }

    "human player gets to shoot two times and wins" should {
      val testBoardHuman = Board(matrixHuman, singleShipHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, doubleShipAi, doubleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      var counter = 0

      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => {
        counter = counter + 1

        if (counter == 1)
          "00"
        else
          "01"
      }, () => "11")

      "show human as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().get)
      }
    }

    "human player gets to shoot two times and fails to input correct coordinates" should {
      val testBoardHuman = Board(matrixHuman, singleShipHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, doubleShipAi, doubleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "00", () => "11")

      "fails" in {
        assert(gameAfterPlaying.isFailure)
      }
    }

    "human player wins after second round" should {
      val testBoardHuman = Board(matrixHuman, singleShipHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, doubleShipAi, doubleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      var counter = 0

      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => {
        counter = counter + 1

        if (counter == 1)
          "00"
        else if (counter == 2)
          "55"
        else
          "01"
      }, () => "11")

      "show human as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner().get)
      }
    }
  }
}
