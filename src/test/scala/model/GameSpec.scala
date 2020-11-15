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
        assert(game.humanPlayerBoard.ships == p1Ships)
      }
      "have player 2 board with player 2 ships" in {
        assert(game.aiPlayerBoard.ships == p2Ships)
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
        val result = testGame.isRunning
        assert(result.isSuccess)
        assert(!result.get)
      }

      "declare ai as winner" in {
        assert(!testGame.humanPlayerIsWinner())
      }
    }

    "ship positions are inconsistent" should {
      val shipsHuman = Vector(
        Ship(2, "humanShip1")
      )
      val shipsAi = Vector(
        Ship(2, "aiShip1")
      )

      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixHuman2 = matrixHuman.updated(3, matrixHuman(3).updated(4, BoardCell(true)))
      val matrixHuman3 = matrixHuman2.updated(3, matrixHuman2(3).updated(5, BoardCell(true)))

      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

      "human ship positions are inconsistent" should {
        val shipPositionsHuman = Vector()

        val shipPositionsAi = Vector(
          ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
        )

        val testBoardHuman = Board(matrixHuman3, shipsHuman, shipPositionsHuman)
        val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

        val testGame = Game(testBoardHuman, testBoardAi, 10)

        "fail on isRunning check" in {
          assert(testGame.isRunning.isFailure)
        }
      }

      "ai ship positions are inconsistent" should {
        val shipPositionsHuman = Vector(
          ShipPosition(shipsHuman(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
        )

        val shipPositionsAi = Vector()

        val testBoardHuman = Board(matrixHuman3, shipsHuman, shipPositionsHuman)
        val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

        val testGame = Game(testBoardHuman, testBoardAi, 10)

        "fail on isRunning check" in {
          assert(testGame.isRunning.isFailure)
        }
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
        val result = testGame.isRunning
        assert(result.isSuccess)
        assert(!result.get)
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
        val result = testGame.isRunning
        assert(result.isSuccess)
        assert(result.get)
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
      val game = new Game(p1Ships, p2Ships)
      val gameWithPlacedShips = game.placeAllShipsRandomly((_: Int) => 2)

      "still have the same round number" in {
        assert(gameWithPlacedShips.isSuccess)
        assert(game.roundNum == gameWithPlacedShips.get.roundNum)
      }

      "have placed all ships correctly" in {
        assert(gameWithPlacedShips.get.humanPlayerBoard.shipPositions.length == 1)
        assert(gameWithPlacedShips.get.humanPlayerBoard.shipPositions(0).ship == p1Ships(0))
        assert(gameWithPlacedShips.get.humanPlayerBoard.shipPositions(0).positions.length == 1)
        assert(gameWithPlacedShips.get.humanPlayerBoard.shipPositions(0).positions(0).row == 2)
        assert(gameWithPlacedShips.get.humanPlayerBoard.shipPositions(0).positions(0).col == 2)

        assert(gameWithPlacedShips.get.aiPlayerBoard.shipPositions.length == 1)
        assert(gameWithPlacedShips.get.aiPlayerBoard.shipPositions(0).ship == p2Ships(0))
        assert(gameWithPlacedShips.get.aiPlayerBoard.shipPositions(0).positions.length == 1)
        assert(gameWithPlacedShips.get.aiPlayerBoard.shipPositions(0).positions(0).row == 2)
        assert(gameWithPlacedShips.get.aiPlayerBoard.shipPositions(0).positions(0).col == 2)
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
        val gameFailHuman = new Game(p1Ships, p2Ships)
        val gameFailHumanResult = gameFailHuman.placeAllShipsRandomly((_: Int) => 2)

        "fail" in {
          assert(gameFailHumanResult.isFailure)
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
        val gameFailAi = new Game(p1Ships, p2Ships)
        val gameFailAiResult = gameFailAi.placeAllShipsRandomly((_: Int) => 2)

        "fail" in {
          assert(gameFailAiResult.isFailure)
        }
      }
    }
  }

  "Human player board" when {
    val ships = Vector(
      Ship(2, "Test 1")
    )
    val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
    val shipPositions = Vector(
      ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
    )
    val board = Board(matrix, ships, shipPositions)

    "shot at" should {
      val game = Game(board, null, 1)
      val shootResult = game.shootAtBoard(humanPlayerBoard = true, 3, 4)

      "register BoardCell hit" in {
        assert(shootResult.isSuccess)
        assert(shootResult.get._1.humanPlayerBoard.matrix(3)(4).isHit)
      }

      "shot at different BoardCell" should {
        val shoot2Result = shootResult.get._1.shootAtBoard(humanPlayerBoard = true, 8, 1)

        "register both BoardCell hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get._1.humanPlayerBoard.matrix(8)(1).isHit)
          assert(shoot2Result.get._1.humanPlayerBoard.matrix(3)(4).isHit) // First shot
        }

        "register no ship hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get._2.isEmpty)
        }

        "register ship hit" in {
          assert(shootResult.isSuccess)
          assert(shootResult.get._2.isDefined)
        }
      }

      "shot at same BoardCell again" should {
        val result3Shot = shootResult.get._1.shootAtBoard(humanPlayerBoard = true, 3, 4)

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
    val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
    val shipPositions = Vector(
      ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
    )
    val board = Board(matrix, ships, shipPositions)

    "shot at" should {
      val game = Game(null, board, 1)
      val shootResult = game.shootAtBoard(humanPlayerBoard = false, 3, 4)

      "register BoardCell hit" in {
        assert(shootResult.isSuccess)
        assert(shootResult.get._1.aiPlayerBoard.matrix(3)(4).isHit)
      }

      "shot at different BoardCell" should {
        val shoot2Result = shootResult.get._1.shootAtBoard(humanPlayerBoard = false, 8, 1)

        "register both BoardCell hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get._1.aiPlayerBoard.matrix(8)(1).isHit)
          assert(shoot2Result.get._1.aiPlayerBoard.matrix(3)(4).isHit) // First shot
        }

        "register no ship hit" in {
          assert(shoot2Result.isSuccess)
          assert(shoot2Result.get._2.isEmpty)
        }

        "register ship hit" in {
          assert(shootResult.isSuccess)
          assert(shootResult.get._2.isDefined)
        }
      }

      "shot at same BoardCell again" should {
        val result3Shot = shootResult.get._1.shootAtBoard(humanPlayerBoard = false, 3, 4)

        "fail" in {
          assert(result3Shot.isFailure)
        }
      }
    }
  }
}
