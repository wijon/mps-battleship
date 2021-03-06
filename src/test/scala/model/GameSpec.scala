package model

import dataTransferObjects.{Coordinates, Ship, ShipPosition}
import enums.{BoardDirection, Player}
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
      val game = new Game(p1Ships, p2Ships, (_: Int) => 3)

      "have player 1 board with player 1 ships" in {
        assert(game.humanPlayerBoard.shipPositions.map(_.ship) == p1Ships)
      }

      "have player 2 board with player 2 ships" in {
        assert(game.aiPlayerBoard.shipPositions.map(_.ship) == p2Ships)
      }
    }

    "starting a new round" should {
      val p1Ships = Vector(
        Ship(1, "A")
      )
      val p2Ships = Vector(
        Ship(2, "B")
      )
      val game = new Game(p1Ships, p2Ships, (_: Int) => 3)
      val gameNextRound = game.startNewRound()

      "have a round number which is 1 greater than last round" in {
        assert((game.roundNum + 1) == gameNextRound.roundNum)
      }
    }

    "build with DSL" should {
      val game = Game.newGame { game =>
        game ships { ship =>
          ship place "Submarine" length 3 at 14 facing BoardDirection.West as Player.Player1
          ship place "Submarine" length 3 at 14 facing BoardDirection.West as Player.Player2
        }
      }

      "contain board with correct ship positions for player 1" in {
        val shipPositions = game.humanPlayerBoard.shipPositions
        assert(shipPositions.length == 1)

        val shipPosition = shipPositions(0)
        assert(shipPosition.ship == Ship(3, "Submarine"))
        assert(shipPosition.positions == Vector(Coordinates(1, 4), Coordinates(1, 3), Coordinates(1, 2)))
      }

      "contain board with correct ship positions for player 2" in {
        val shipPositions = game.aiPlayerBoard.shipPositions
        assert(shipPositions.length == 1)

        val shipPosition = shipPositions(0)
        assert(shipPosition.ship == Ship(3, "Submarine"))
        assert(shipPosition.positions == Vector(Coordinates(1, 4), Coordinates(1, 3), Coordinates(1, 2)))
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
        dataTransferObjects.ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixHuman2 = matrixHuman.updated(3, matrixHuman(3).updated(4, true))
      val matrixHuman3 = matrixHuman2.updated(3, matrixHuman2(3).updated(5, true))

      val matrixAi = Vector.tabulate(10, 10) { (_, _) => false }

      val testBoardHuman = Board(matrixHuman3, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "not be running" in {
        assert(!testGame.isRunning)
      }

      "declare ai as winner" in {
        assert(!testGame.humanPlayerIsWinner())
      }
    }

    "human player has no ships" should {
      val shipsAi = Vector(
        Ship(2, "aiShip1")
      )

      val shipPositionsHuman = Vector.empty
      val shipPositionsAi = Vector(
        dataTransferObjects.ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => false }

      val testBoardHuman = Board(matrixHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "not be running" in {
        assert(!testGame.isRunning)
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
        dataTransferObjects.ShipPosition(shipsHuman(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val shipPositionsAi = Vector(
        dataTransferObjects.ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixAi2 = matrixAi.updated(3, matrixAi(3).updated(4, true))
      val matrixAi3 = matrixAi2.updated(3, matrixAi2(3).updated(5, true))

      val testBoardHuman = Board(matrixHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi3, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "not be running" in {
        assert(!testGame.isRunning)
      }

      "declare human as winner" in {
        assert(testGame.humanPlayerIsWinner())
      }
    }

    "ai player has no ships" should {
      val shipsHuman = Vector(
        Ship(2, "humanShip1")
      )

      val shipPositionsHuman = Vector(
        dataTransferObjects.ShipPosition(shipsHuman(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val shipPositionsAi = Vector.empty

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => false }

      val testBoardHuman = Board(matrixHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "not be running" in {
        assert(!testGame.isRunning)
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
        dataTransferObjects.ShipPosition(shipsHuman(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val shipPositionsAi = Vector(
        dataTransferObjects.ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => false }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => false }

      val testBoardHuman = Board(matrixHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)

      "be running" in {
        assert(testGame.isRunning)
      }

      "human player is not winner" in {
        assert(!testGame.humanPlayerIsWinner())
      }
    }

    "placing ships randomly on boards" should {
      val p1Ships = Vector(
        Ship(1, "A")
      )
      val p2Ships = Vector(
        Ship(1, "B")
      )
      val gameWithPlacedShips = new Game(p1Ships, p2Ships, (_: Int) => 2)

      "have placed all ships correctly" in {
        assert(gameWithPlacedShips.humanPlayerBoard.shipPositions.length == 1)
        assert(gameWithPlacedShips.humanPlayerBoard.shipPositions(0).ship == p1Ships(0))
        assert(gameWithPlacedShips.humanPlayerBoard.shipPositions(0).positions.length == 1)
        assert(gameWithPlacedShips.humanPlayerBoard.shipPositions(0).positions(0).row == 2)
        assert(gameWithPlacedShips.humanPlayerBoard.shipPositions(0).positions(0).col == 2)

        assert(gameWithPlacedShips.aiPlayerBoard.shipPositions.length == 1)
        assert(gameWithPlacedShips.aiPlayerBoard.shipPositions(0).ship == p2Ships(0))
        assert(gameWithPlacedShips.aiPlayerBoard.shipPositions(0).positions.length == 1)
        assert(gameWithPlacedShips.aiPlayerBoard.shipPositions(0).positions(0).row == 2)
        assert(gameWithPlacedShips.aiPlayerBoard.shipPositions(0).positions(0).col == 2)
      }

      "when coordinates on human player board are already blocked" should {
        val p1Ships = Vector(
          Ship(1, "A"),
          Ship(1, "B"),
          Ship(1, "C")
        )
        val p2Ships = Vector(
          Ship(1, "D")
        )

        "fail" in {
          assertThrows[IndexOutOfBoundsException](new Game(p1Ships, p2Ships, (_: Int) => 2))
        }
      }

      "when coordinates on ai player board are already blocked" should {
        val p1Ships = Vector(
          Ship(1, "D")
        )
        val p2Ships = Vector(
          Ship(1, "A"),
          Ship(1, "B"),
          Ship(1, "C")
        )

        "fail" in {
          assertThrows[IndexOutOfBoundsException](new Game(p1Ships, p2Ships, (_: Int) => 2))
        }
      }
    }
  }

  "Human player board" when {
    val ships = Vector(
      Ship(2, "Test 1")
    )
    val matrix = Vector.tabulate(10, 10) { (_, _) => false }
    val shipPositions = Vector(
      dataTransferObjects.ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
    )
    val board = Board(matrix, shipPositions)

    "shot at" should {
      val game = Game(board, null, 1)
      val shootResult = game.shootAtBoard(humanPlayerBoard = true, 3, 4)

      "register BoardCell hit" in {
        assert(shootResult.isSuccess)
        assert(shootResult.get.game.humanPlayerBoard.matrix(3)(4))
      }

      "shot at different BoardCell" should {
        val shoot2Result = shootResult.get.game.shootAtBoard(humanPlayerBoard = true, 8, 1)

        "register both BoardCell hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get.game.humanPlayerBoard.matrix(8)(1))
          assert(shoot2Result.get.game.humanPlayerBoard.matrix(3)(4)) // First shot
        }

        "register no ship hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get.shipPosition.isEmpty)
        }

        "register ship hit" in {
          assert(shootResult.isSuccess)
          assert(shootResult.get.shipPosition.isDefined)
        }
      }

      "shot at same BoardCell again" should {
        val result3Shot = shootResult.get.game.shootAtBoard(humanPlayerBoard = true, 3, 4)

        "fail" in {
          assert(result3Shot.isFailure)
        }
      }
    }
  }

  "Ai player board" when {
    val ships = Vector(
      Ship(2, "Test 1")
    )
    val matrix = Vector.tabulate(10, 10) { (_, _) => false }
    val shipPositions = Vector(
      dataTransferObjects.ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
    )
    val board = Board(matrix, shipPositions)

    "shot at" should {
      val game = Game(null, board, 1)
      val shootResult = game.shootAtBoard(humanPlayerBoard = false, 3, 4)

      "register BoardCell hit" in {
        assert(shootResult.isSuccess)
        assert(shootResult.get.game.aiPlayerBoard.matrix(3)(4))
      }

      "shot at different BoardCell" should {
        val shoot2Result = shootResult.get.game.shootAtBoard(humanPlayerBoard = false, 8, 1)

        "register both BoardCell hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get.game.aiPlayerBoard.matrix(8)(1))
          assert(shoot2Result.get.game.aiPlayerBoard.matrix(3)(4)) // First shot
        }

        "register no ship hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get.shipPosition.isEmpty)
        }

        "register ship hit" in {
          assert(shootResult.isSuccess)
          assert(shootResult.get.shipPosition.isDefined)
        }
      }

      "shot at same BoardCell again" should {
        val result3Shot = shootResult.get.game.shootAtBoard(humanPlayerBoard = false, 3, 4)

        "fail" in {
          assert(result3Shot.isFailure)
        }
      }
    }
  }
}
