package model

import org.scalatest.wordspec.AnyWordSpec

class OutputHelperSpec extends AnyWordSpec {
  "Final text" when {
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

    "Game is still running" should {
      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

      val testBoardHuman = Board(matrixHuman, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)
      val outputTest = OutputHelper.generateFinalText(testGame)

      "Fail" in {
        assert(outputTest.isFailure)
      }
    }

    "Human player is winner" should {
      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixAi2 = matrixAi.updated(3, matrixAi(3).updated(4, BoardCell(true)))
      val matrixAi3 = matrixAi2.updated(3, matrixAi2(3).updated(5, BoardCell(true)))

      val testBoardHuman = Board(matrixHuman, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi3, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)
      val outputTest = OutputHelper.generateFinalText(testGame)

      val victoryText = OutputHelper.generateVictory()

      "Give victory text" in {
        assert(outputTest.isSuccess)
        assert(outputTest.get.mkString(" ") == victoryText.mkString(" "))
      }
    }

    "Human player is looser" should {
      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixHuman2 = matrixHuman.updated(3, matrixHuman(3).updated(4, BoardCell(true)))
      val matrixHuman3 = matrixHuman2.updated(3, matrixHuman2(3).updated(5, BoardCell(true)))
      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

      val testBoardHuman = Board(matrixHuman3, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)
      val outputTest = OutputHelper.generateFinalText(testGame)

      val lossText = OutputHelper.generateLoss()

      "Give loss text" in {
        assert(outputTest.isSuccess)
        assert(outputTest.get.mkString(" ") == lossText.mkString(" "))
      }
    }

    "Game is inconsistent" should {
      val matrixHuman = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val matrixHuman2 = matrixHuman.updated(3, matrixHuman(3).updated(4, BoardCell(true)))
      val matrixHuman3 = matrixHuman2.updated(3, matrixHuman2(3).updated(5, BoardCell(true)))

      val matrixAi = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }

      val shipPositionsHuman = Vector()

      val shipPositionsAi = Vector(
        ShipPosition(shipsAi(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val testBoardHuman = Board(matrixHuman3, shipsHuman, shipPositionsHuman)
      val testBoardAi = Board(matrixAi, shipsAi, shipPositionsAi)

      val testGame = Game(testBoardHuman, testBoardAi, 10)
      val outputTest = OutputHelper.generateFinalText(testGame)

      "Fail" in {
        assert(outputTest.isFailure)
      }
    }

  }

  "Victory view" when {
    "called" should {
      val textAsString = OutputHelper.generateVictory().mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain victory text" in {
        assert(textAsString.contains("SIE HABEN GEWONNEN"))
      }

      "contain a happy face" in {
        assert(textAsString.contains(":)"))
      }
    }
  }

  "Loss view" when {
    "called" should {
      val textAsString = OutputHelper.generateLoss().mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain victory text" in {
        assert(textAsString.contains("SIE HABEN VERLOREN"))
      }

      "contain a sad face" in {
        assert(textAsString.contains(":("))
      }
    }
  }

  "Round info text view" when {
    "called" should {
      val roundNumber = 53
      val game = Game(null, null, roundNumber)
      val textAsString = OutputHelper.generateRoundInfoText(game).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain round number" in {
        val regex = "^.*(Runde " + roundNumber + ").*$"
        assert(textAsString.matches(regex))
      }
    }
  }

  "Shoot info text" when {
    "called" should {
      val testCoordinates = Coordinates(3, 7)
      val textAsString = OutputHelper.generateShootInfoText(testCoordinates.row, testCoordinates.col).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain the coordinates" in {
        val regex = "^.*" + testCoordinates.row + testCoordinates.col + ".*$"
        assert(textAsString.matches(regex))
      }
    }
  }

  "Ship hit info text" when {
    "called" should {
      val testShip = Ship(3, "TestShip")
      val textAsString = OutputHelper.generateShipHitInfotext(testShip).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain ship name" in {
        assert(textAsString.contains(testShip.name))
      }
    }
  }

  "Ship destroyed info text" when {
    "called" should {
      val testShip = Ship(3, "TestShip")
      val textAsString = OutputHelper.generateShipDestroyedInfoText(testShip).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain ship name" in {
        assert(textAsString.contains(testShip.name))
      }
    }
  }

  "Remaining ship info text" when {
    "called with multiple ships on the board" should {
      val ships = Vector(
        Ship(2, "Test 1"),
        Ship(3, "Test 2")
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
        ShipPosition(ships(1), Vector(Coordinates(1, 1), Coordinates(1, 2), Coordinates(1, 3)))
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
      val matrix2 = matrix1.updated(3, matrix1(3).updated(5, BoardCell(true)))
      val matrix3 = matrix2.updated(1, matrix2(1).updated(2, BoardCell(true)))
      val testBoard = Board(matrix3, ships, shipPositions)

      val test = OutputHelper.generateRemainingShips(testBoard, "human")

      "contain one line for every ship on the board and one headline" in {
        assert(test.length == (ships.length + 1))
      }
    }

    "called with one ship on the board, that is not hit" should {
      val shipName = "test 123"

      val ships = Vector(
        Ship(2, shipName)
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val testBoard = Board(matrix, ships, shipPositions)

      val test = OutputHelper.generateRemainingShips(testBoard,"human")

      "show this ship correctly" in {
        assert(test.mkString(" ").contains("\\__/"))
      }

      "show the name of the ship" in {
        assert(test.mkString(" ").contains(shipName))
      }

      "show 0 hits" in {
        assert(test.mkString(" ").contains("0 hit(s)"))
      }
    }

    "called with one hit ship on the board" should {
      val shipName = "test 123"

      val ships = Vector(
        Ship(2, shipName)
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(5, BoardCell(true)))
      val testBoard = Board(matrix1, ships, shipPositions)

      val test = OutputHelper.generateRemainingShips(testBoard, "human")

      "show this ship correctly" in {
        assert(test.mkString(" ").contains("\\_X/"))
      }

      "show the name of the ship" in {
        assert(test.mkString(" ").contains(shipName))
      }

      "show 1 hit" in {
        assert(test.mkString(" ").contains("1 hit(s)"))
      }
    }

    "called with one destroyed ship on the board" should {
      val shipName = "test 123"

      val ships = Vector(
        Ship(2, shipName)
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
      val matrix2 = matrix1.updated(3, matrix1(3).updated(5, BoardCell(true)))
      val testBoard = Board(matrix2, ships, shipPositions)

      val test = OutputHelper.generateRemainingShips(testBoard, "human")

      "show this ship correctly" in {
        assert(test.mkString(" ").contains("\\XX/"))
      }

      "show the name of the ship" in {
        assert(test.mkString(" ").contains(shipName))
      }

      "show 2 hits" in {
        assert(test.mkString(" ").contains("2 hit(s)"))
      }

      "show destroyed" in {
        assert(test.mkString(" ").contains("destroyed"))
      }
    }
  }

  "Board text" when {
    "called with ships to show" should {
      val ships = Vector(
        Ship(2, "Test 1"),
        Ship(3, "Test 2"),
        Ship(5, "Test 2"),
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
        ShipPosition(ships(1), Vector(Coordinates(1, 1), Coordinates(1, 2), Coordinates(1, 3))),
        ShipPosition(ships(2), Vector(Coordinates(9, 0), Coordinates(8, 0), Coordinates(7, 0), Coordinates(6, 0), Coordinates(5, 0))),
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
      val matrix2 = matrix1.updated(3, matrix1(3).updated(5, BoardCell(true)))
      val matrix3 = matrix2.updated(1, matrix2(1).updated(2, BoardCell(true)))
      val matrix4 = matrix3.updated(9, matrix3(9).updated(8, BoardCell(true)))
      val matrix5 = matrix4.updated(7, matrix4(7).updated(6, BoardCell(true)))
      val testBoard = Board(matrix5, ships, shipPositions)

      val test = OutputHelper.generateBoard(testBoard, showShips = true, "human")

      "not be empty" in {
        assert(!test.mkString(" ").isEmpty)
      }

      "show ships" in {
        assert(test.mkString(" ").contains("O"))
      }

      "show hits in water" in {
        assert(test.mkString(" ").contains("X"))
      }

      "show hits on boat" in {
        assert(test.mkString(" ").contains("@"))
      }
    }

    "called with ships to hide" should {
      val ships = Vector(
        Ship(2, "Test 1"),
        Ship(3, "Test 2"),
        Ship(5, "Test 2"),
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
        ShipPosition(ships(1), Vector(Coordinates(1, 1), Coordinates(1, 2), Coordinates(1, 3))),
        ShipPosition(ships(2), Vector(Coordinates(9, 0), Coordinates(8, 0), Coordinates(7, 0), Coordinates(6, 0), Coordinates(5, 0))),
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
      val matrix2 = matrix1.updated(3, matrix1(3).updated(5, BoardCell(true)))
      val matrix3 = matrix2.updated(1, matrix2(1).updated(2, BoardCell(true)))
      val matrix4 = matrix3.updated(9, matrix3(9).updated(8, BoardCell(true)))
      val matrix5 = matrix4.updated(7, matrix4(7).updated(6, BoardCell(true)))
      val testBoard = Board(matrix5, ships, shipPositions)

      val test = OutputHelper.generateBoard(testBoard, showShips = false, "ai")

      "not be empty" in {
        assert(!test.mkString(" ").isEmpty)
      }

      "not show ships" in {
        assert(!test.mkString(" ").contains("O"))
      }

      "show hits in water" in {
        assert(test.mkString(" ").contains("X"))
      }

      "show hits on boat" in {
        assert(test.mkString(" ").contains("@"))
      }
    }
  }
}
