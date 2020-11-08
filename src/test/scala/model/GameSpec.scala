package model

import org.scalatest.wordspec.AnyWordSpec

class GameSpec extends AnyWordSpec {
  "A Game" when {
    "new" should {
      val p1Ships = Vector(
        Ship(1, "A")
      )
      val p2Ships = Vector(
        Ship(2, "B")
      )
      val game = new Game(p1Ships, p2Ships)
      "have player 1 board with player 1 ships" in {
        assert(game.player1Board.ships == p1Ships)
      }
      "have player 2 board with player 2 ships" in {
        assert(game.player2Board.ships == p2Ships)
      }
    }

    "all ships of human player are destroyed" should {
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
      val matrixHuman2 = matrixHuman.updated(3, matrixHuman(3).updated(4, BoardCell(true)))
      val matrixHuman3 = matrixHuman2.updated(3, matrixHuman2(3).updated(5, BoardCell(true)))

      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

      val testBoardHuman = Board(matrixHuman3, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "not be running" in {
        assert(!testGame.isRunning())
      }

      "declare ai as winner" in {
        assert(!testGame.humanPlayerIsWinner())
      }
    }

    "all ships of ai player are destroyed" should {
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
      val matrixAi2 = matrixAi.updated(3, matrixAi(3).updated(4, BoardCell(true)))
      val matrixAi3 = matrixAi2.updated(3, matrixAi2(3).updated(5, BoardCell(true)))

      val testBoardHuman = Board(matrixHuman, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi3, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "not be running" in {
        assert(!testGame.isRunning())
      }

      "declare human as winner" in {
        assert(testGame.humanPlayerIsWinner())
      }
    }

    "both players have at least one not-destroyed ship" should {
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

      "be running" in {
        assert(testGame.isRunning())
      }
    }
  }
}
