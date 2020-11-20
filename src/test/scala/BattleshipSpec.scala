import dataTransferObjects.{BoardCell, Coordinates, Ship, ShipPosition}
import model.{Board, Game}
import org.scalatest.wordspec.AnyWordSpec

class BattleshipSpec extends AnyWordSpec {
  "Battleship" when {
    "check input for correct coordinates" should {
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixWithHit = matrix.updated(4, matrix(4).updated(2, BoardCell(true)))
      val boardWithHitAt42 = Board(matrixWithHit, Vector())

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
      val board = Board(Vector(Ship(2, "Test")), (_: Int) => 1)

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

  "Playing the game" when {
    val singleShipHuman = Vector(
      Ship(1, "humanShip1")
    )
    val doubleShipHuman = Vector(
      Ship(2, "humanShip2")
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

    val doubleShipPositionsHuman = Vector(
      dataTransferObjects.ShipPosition(doubleShipHuman(0), Vector(Coordinates(0, 0), Coordinates(0, 1))),
    )

    val singleShipPositionsAi = Vector(
      dataTransferObjects.ShipPosition(singleShipAi(0), Vector(Coordinates(0, 0))),
    )

    val doubleShipPositionsAi = Vector(
      dataTransferObjects.ShipPosition(doubleShipAi(0), Vector(Coordinates(0, 0), Coordinates(0, 1))),
    )

    val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
    val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

    "human player is winning" should {
      val testBoardHuman = Board(matrixHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "00", () => "11", 0)

      "show human as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner())
      }
    }

    "ai player is winning" should {
      val testBoardHuman = Board(matrixHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "11", () => "00", 0)

      "show ai as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(!gameAfterPlaying.get.humanPlayerIsWinner())
      }
    }

    "max number of wrong inputs exceeds" should {
      val testBoardHuman = Board(matrixHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "11", () => "11", 0)

      "fail" in {
        assert(gameAfterPlaying.isFailure)
      }
    }

    "human player gets to shoot two times and wins" should {
      val testBoardHuman = Board(matrixHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, doubleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      var counter = 0

      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => {
        counter = counter + 1

        if (counter == 1)
          "00"
        else
          "01"
      }, () => "11", 0)

      "show human as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner())
      }
    }

    "human player gets to shoot two times and fails to input correct coordinates" should {
      val testBoardHuman = Board(matrixHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, doubleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "00", () => "11", 0)

      "fails" in {
        assert(gameAfterPlaying.isFailure)
      }
    }

    "ai player gets to shoot two times and fails to input correct coordinates" should {
      val testBoardHuman = Board(matrixHuman, doubleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, singleShipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 0)
      val gameAfterPlaying = Battleship.play((_: String) => (), testGame, () => "11", () => "00", 0)

      "fails" in {
        assert(gameAfterPlaying.isFailure)
      }
    }

    "human player wins after second round" should {
      val testBoardHuman = Board(matrixHuman, singleShipPositionsHuman)
      val testBoardAi = Board(matrixAi, doubleShipPositionsAi)

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
      }, () => "11", 0)

      "show human as winner" in {
        assert(gameAfterPlaying.isSuccess)
        assert(gameAfterPlaying.get.humanPlayerIsWinner())
      }
    }
  }
}
