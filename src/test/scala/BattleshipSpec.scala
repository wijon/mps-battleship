import dataTransferObjects.{Coordinates, Ship, ShipPosition}
import model.{Board, Game}
import org.scalatest.wordspec.AnyWordSpec

class BattleshipSpec extends AnyWordSpec {
  "Battleship" when {

    // Currently not testable because of how play is working
    //    "instantiated with ship positions" should {
    //
    //      "not fail" in {
    //        val txt = "Player: 1\nName\t\tLength\tStartingPos\tDirection\nCarrier\t\t5\t14\t\tEast\nBattleship\t4\t23\t\tSouth\n\nPlayer: 2\nName\t\tLength\tStartingPos\tDirection\n"
    //        Battleship.main(Array(txt))
    //      }
    //    }

    "create game with explicit ships" should {
      val txt = "Player: 1\nName\t\tLength\tStartingPos\tDirection\nCarrier\t\t5\t14\t\tEast\nBattleship\t4\t23\t\tSouth\n\nPlayer: 2\nName\t\tLength\tStartingPos\tDirection\nCarrier\t\t5\t38\t\tWest\nBattleship\t4\t72\t\tNorth"
      val game = Battleship.createGameWithExplicitShips(txt)

      "have human board" should {
        val board = game.humanPlayerBoard

        "be initialized" in {
          assert(board.matrix.flatten(m => m).length == 100)
        }

        "contain given ships" in {
          val ships = board.shipPositions.map(sp => sp.ship)
          assert(ships.length == 2)
          assert(ships.contains(Ship(5, "Carrier")))
          assert(ships.contains(Ship(4, "Battleship")))
        }

        "contain ships at correct position" in {
          val carrierPos = board.shipPositions.find(sp => sp.ship == Ship(5, "Carrier")).get.positions
          assert(carrierPos.length == 5)
          assert(carrierPos == Vector(
            Coordinates(1, 4),
            Coordinates(1, 5),
            Coordinates(1, 6),
            Coordinates(1, 7),
            Coordinates(1, 8))
          )

          val battleshipPos = board.shipPositions.find(sp => sp.ship == Ship(4, "Battleship")).get.positions
          assert(battleshipPos.length == 4)
          assert(battleshipPos == Vector(
            Coordinates(2, 3),
            Coordinates(3, 3),
            Coordinates(4, 3),
            Coordinates(5, 3))
          )
        }
      }

      "have ai board" should {
        val board = game.aiPlayerBoard

        "be initialized" in {
          assert(board.matrix.flatten(m => m).length == 100)
        }

        "contain given ships" in {
          val ships = board.shipPositions.map(sp => sp.ship)
          assert(ships.length == 2)
          assert(ships.contains(Ship(5, "Carrier")))
          assert(ships.contains(Ship(4, "Battleship")))
        }

        "contain ships at correct position" in {
          val carrierPos = board.shipPositions.find(sp => sp.ship == Ship(5, "Carrier")).get.positions
          assert(carrierPos.length == 5)
          assert(carrierPos == Vector(
            Coordinates(3, 8),
            Coordinates(3, 7),
            Coordinates(3, 6),
            Coordinates(3, 5),
            Coordinates(3, 4))
          )

          val battleshipPos = board.shipPositions.find(sp => sp.ship == Ship(4, "Battleship")).get.positions
          assert(battleshipPos.length == 4)
          assert(battleshipPos == Vector(
            Coordinates(7, 2),
            Coordinates(6, 2),
            Coordinates(5, 2),
            Coordinates(4, 2))
          )
        }
      }
    }

    "check input for correct coordinates" should {
      val matrix = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixWithHit = matrix.updated(4, matrix(4).updated(2, true))
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

    val matrixHuman = Vector.tabulate(10, 10) { (_, _) => false }
    val matrixAi = Vector.tabulate(10, 10) { (_, _) => false }

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
