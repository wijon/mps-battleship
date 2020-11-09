package model

import org.scalatest.wordspec.AnyWordSpec

class BoardSpec extends AnyWordSpec {
  "A Board" when {
    "to be constructed" should {
      val ships = Vector(
        Ship(1, "Test 1"),
        Ship(2, "Test 2")
      )
      val board = new Board(ships)

      "have ships" in {
        assert(board.ships == ships)
      }
      "have a 10x10 matrix" in {
        assert(board.matrix.length == 10 && board.matrix(0).length == 10)
      }
      "have no hits registered" in {
        assert(board.matrix.flatten.forall(bc => !bc.isHit))
      }
    }

    "shot at" should {
      val ships = Vector(
        Ship(2, "Test 1")
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
      )
      val board = Board(matrix, ships, shipPositions)
      val result1Shot = board.shoot(3, 4)

      "register BoardCell hit" in {
        assert(result1Shot.isSuccess)
        assert(result1Shot.get.board.matrix(3)(4).isHit)
      }

      "shot at different BoardCell" should {
        val result2Shot = result1Shot.get.board.shoot(8, 1)

        "register both BoardCell hit" in {
          assert(result2Shot.isSuccess)
          assert(result2Shot.get.board.matrix(8)(1).isHit)
          assert(result2Shot.get.board.matrix(3)(4).isHit) // First shot
        }

        "register no ship hit" in {
          assert(result2Shot.isSuccess)
          assert(!result2Shot.get.isShipHit)
        }

        "register ship hit" in {
          assert(result1Shot.isSuccess)
          assert(result1Shot.get.isShipHit)
        }
      }

      "shot at same BoardCell again" should {
        val result3Shot = result1Shot.get.board.shoot(3, 4)

        "fail" in {
          assert(result3Shot.isFailure)
        }
      }
    }

    "ship destroyed" should {
      val ships = Vector(
        Ship(2, "Test 1")
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
      val matrix2 = matrix1.updated(3, matrix1(3).updated(5, BoardCell(true)))
      val board = Board(matrix2, ships, shipPositions)

      "ship is destroyed" in {
        val result = board.isDestroyed(ships(0))
        assert(result.isSuccess)
        assert(result.get)
      }
    }

    "ship not destroyed" should {
      val ships = Vector(
        Ship(2, "Test 1")
      )
      val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
      val shipPositions = Vector(
        ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5)))
      )

      val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
      val board = Board(matrix1, ships, shipPositions)

      "ship is not destroyed" in {
        val result = board.isDestroyed(ships(0))
        assert(result.isSuccess)
        assert(!result.get)
      }

      "ship not found" in {
        val result = board.isDestroyed(Ship(1, "404"))
        assert(result.isFailure)
      }
    }

    "ship is placed on empty board by coordinates" should {
      val ship = Ship(2, "TestShip1")
      val shipCoordinates = Vector(Coordinates(3, 4), Coordinates(3, 5))
      val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)
      val newBoard = board.placeSingleShip(ship, shipCoordinates)

      "know ship position" in {
        assert(newBoard.isSuccess)
        assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship) && sp.positions.equals(shipCoordinates)))
      }
    }

    "ship is placed on not empty board by coordinates" should {
      val ship = Ship(2, "TestShip1")
      val shipCoordinates = Vector(Coordinates(3, 4), Coordinates(3, 5))
      val ship2 = Ship(2, "TestShip2")
      val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
      val ship2Position = ShipPosition(ship2, ship2Coordinates)
      val board =
        Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship2), Vector(ship2Position))
      val newBoard = board.placeSingleShip(ship, shipCoordinates)

      "know new ship position" in {
        assert(newBoard.isSuccess)
        assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship) && sp.positions.equals(shipCoordinates)))
      }

      "still know position of old ship" in {
        assert(newBoard.isSuccess)
        assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
      }
    }

    "ship is placed on empty board by coordinate and direction" should {
      "when direction is north" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.North
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(4, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(0, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(0, 5)
          val position2 = Coordinates(1, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }

      "when direction is east" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.East
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 6)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(5, 9)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 9)
          val position2 = Coordinates(5, 8)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }

      "when direction is south" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.South
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(6, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(9, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(9, 5)
          val position2 = Coordinates(8, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }

      "when direction is west" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.West
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 4)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(5, 0)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 0)
          val position2 = Coordinates(5, 1)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }
    }

    "ship is placed on not empty board by coordinate and direction" should {
      "when direction is north" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.North
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(4, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(0, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(0, 5)
          val position2 = Coordinates(1, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }

      "when direction is east" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.East
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 6)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(5, 9)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 9)
          val position2 = Coordinates(5, 8)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }

      "when direction is south" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.South
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(6, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(9, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(9, 5)
          val position2 = Coordinates(8, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }

      "when direction is west" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.West
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val startCoordinates = Coordinates(5, 5)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 4)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val startCoordinates = Coordinates(5, 0)
          val newBoard = board.placeSingleShip(ship, startCoordinates, shipDirection)

          val position1 = Coordinates(5, 0)
          val position2 = Coordinates(5, 1)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }
    }

    "ship is placed on empty board by functions" should {
      "when direction is north" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.North
        val fktDirection = () => shipDirection
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(4, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 0
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(0, 5)
          val position2 = Coordinates(1, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }

      "when direction is east" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.East
        val fktDirection = () => shipDirection
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 6)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 9

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 9)
          val position2 = Coordinates(5, 8)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }

      "when direction is south" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.South
        val fktDirection = () => shipDirection
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(6, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 9
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(9, 5)
          val position2 = Coordinates(8, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }

      "when direction is west" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.West
        val fktDirection = () => shipDirection
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship), Vector.empty)

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 4)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 0

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 0)
          val position2 = Coordinates(5, 1)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }
        }
      }
    }

    "ship is placed on not empty board by functions" should {
      "when direction is north" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.North
        val fktDirection = () => shipDirection
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(4, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 0
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(0, 5)
          val position2 = Coordinates(1, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }

      "when direction is east" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.East
        val fktDirection = () => shipDirection
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 6)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 9

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 9)
          val position2 = Coordinates(5, 8)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }

      "when direction is south" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.South
        val fktDirection = () => shipDirection
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(6, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 9
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(9, 5)
          val position2 = Coordinates(8, 5)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }

      "when direction is west" should {
        val ship = Ship(2, "TestShip1")
        val shipDirection = BoardDirection.West
        val fktDirection = () => shipDirection
        val ship2 = Ship(2, "TestShip2")
        val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
        val ship2Position = ShipPosition(ship2, ship2Coordinates)
        val board =
          Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship, ship), Vector(ship2Position))

        "when ship is placed" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 5

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 5)
          val position2 = Coordinates(5, 4)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }

        "when ship is placed too close to board boarder" should {
          val fktStartingRow = () => 5
          val fktStartingCol = () => 0

          val newBoard = board.placeSingleShip(ship, fktStartingRow, fktStartingCol, fktDirection)

          val position1 = Coordinates(5, 0)
          val position2 = Coordinates(5, 1)

          "know ship position" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship)
              && sp.positions.exists(p => p.equals(position1))
              && sp.positions.exists(p => p.equals(position2))))
          }

          "still know position of old ship" in {
            assert(newBoard.isSuccess)
            assert(newBoard.get.shipPositions.exists(sp => sp.ship.equals(ship2) && sp.positions.equals(ship2Coordinates)))
          }
        }
      }
    }
  }

  "Generating ship coordinates" when {
    val board = Board(Vector.empty, Vector.empty, Vector.empty)
    val startCoordinate = Coordinates(5, 5)
    val shipLength = 3

    "direction is north" should {
      val coordinates = board.generateShipCoordinates(startCoordinate, shipLength, BoardDirection.North)

      "contain correct number of elements" in {
        assert(coordinates.length == shipLength)
      }

      "contain correct coordinates" in {
        assert(coordinates.exists(c => c.row == 5 && c.col == 5))
        assert(coordinates.exists(c => c.row == 4 && c.col == 5))
        assert(coordinates.exists(c => c.row == 3 && c.col == 5))
      }
    }

    "direction is east" should {
      val coordinates = board.generateShipCoordinates(startCoordinate, shipLength, BoardDirection.East)

      "contain correct number of elements" in {
        assert(coordinates.length == shipLength)
      }

      "contain correct coordinates" in {
        assert(coordinates.exists(c => c.row == 5 && c.col == 5))
        assert(coordinates.exists(c => c.row == 5 && c.col == 6))
        assert(coordinates.exists(c => c.row == 5 && c.col == 7))
      }
    }

    "direction is south" should {
      val coordinates = board.generateShipCoordinates(startCoordinate, shipLength, BoardDirection.South)

      "contain correct number of elements" in {
        assert(coordinates.length == shipLength)
      }

      "contain correct coordinates" in {
        assert(coordinates.exists(c => c.row == 5 && c.col == 5))
        assert(coordinates.exists(c => c.row == 6 && c.col == 5))
        assert(coordinates.exists(c => c.row == 7 && c.col == 5))
      }
    }

    "direction is west" should {
      val coordinates = board.generateShipCoordinates(startCoordinate, shipLength, BoardDirection.West)

      "contain correct number of elements" in {
        assert(coordinates.length == shipLength)
      }

      "contain correct coordinates" in {
        assert(coordinates.exists(c => c.row == 5 && c.col == 5))
        assert(coordinates.exists(c => c.row == 5 && c.col == 4))
        assert(coordinates.exists(c => c.row == 5 && c.col == 3))
      }
    }
  }

  "Placing a ship" when {
    val ship = Ship(2, "TestShip1")
    val shipCoordinates = Vector(Coordinates(3, 4), Coordinates(3, 5))
    val ship2 = Ship(2, "TestShip2")
    val ship2Coordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
    val ship2Position = ShipPosition(ship2, ship2Coordinates)


    "ship does not belong to board" should {
      val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship2), Vector(ship2Position))
      val newBoardByFunction = board.placeSingleShip(ship, () => 0, () => 0, () => BoardDirection.North)
      val newBoardByDirection = board.placeSingleShip(ship, Coordinates(0, 0), BoardDirection.North)
      val newBoardByCoordinates = board.placeSingleShip(ship, shipCoordinates)

      "be failure" in {
        assert(newBoardByFunction.isFailure)
        assert(newBoardByDirection.isFailure)
        assert(newBoardByCoordinates.isFailure)
      }
    }

    "ship is already placed on board" should {
      val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship2), Vector(ship2Position))
      val newBoardByFunction = board.placeSingleShip(ship2, () => 0, () => 0, () => BoardDirection.North)
      val newBoardByDirection = board.placeSingleShip(ship2, Coordinates(0, 0), BoardDirection.North)
      val newBoardByCoordinates = board.placeSingleShip(ship2, shipCoordinates)

      "be failure" in {
        assert(newBoardByFunction.isFailure)
        assert(newBoardByDirection.isFailure)
        assert(newBoardByCoordinates.isFailure)
      }
    }

    "another ship is already placed at coordinates" should {
      val shipCoordinates = Vector(Coordinates(0, 0), Coordinates(0, 1))
      val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) },
        Vector(ship, ship2), Vector(ship2Position))
      val newBoardByFunction = board.placeSingleShip(ship, () => 0, () => 0, () => BoardDirection.East)
      val newBoardByDirection = board.placeSingleShip(ship, Coordinates(0, 0), BoardDirection.East)
      val newBoardByCoordinates = board.placeSingleShip(ship, shipCoordinates)

      "be failure" in {
        assert(newBoardByFunction.isFailure)
        assert(newBoardByDirection.isFailure)
        assert(newBoardByCoordinates.isFailure)
      }
    }

    "coordinates are incorrect" should {
      "row over 9" should {
        val ship2Coordinates = Vector(Coordinates(10, 5), Coordinates(11, 5))
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship2), Vector.empty)
        val newBoardByFunction = board.placeSingleShip(ship, () => 10, () => 5, () => BoardDirection.South)
        val newBoardByDirection = board.placeSingleShip(ship, Coordinates(10, 5), BoardDirection.South)
        val newBoardByCoordinates = board.placeSingleShip(ship2, ship2Coordinates)

        "be failure" in {
          assert(newBoardByFunction.isFailure)
          assert(newBoardByDirection.isFailure)
          assert(newBoardByCoordinates.isFailure)
        }
      }

      "row lower 0" should {
        val ship2Coordinates = Vector(Coordinates(-1, 5), Coordinates(-2, 5))
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship2), Vector.empty)
        val newBoardByFunction = board.placeSingleShip(ship, () => -1, () => 5, () => BoardDirection.North)
        val newBoardByDirection = board.placeSingleShip(ship, Coordinates(-1, 5), BoardDirection.North)
        val newBoardByCoordinates = board.placeSingleShip(ship2, ship2Coordinates)

        "be failure" in {
          assert(newBoardByFunction.isFailure)
          assert(newBoardByDirection.isFailure)
          assert(newBoardByCoordinates.isFailure)
        }
      }

      "col over 9" should {
        val ship2Coordinates = Vector(Coordinates(5, 10), Coordinates(5, 11))
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship2), Vector.empty)
        val newBoardByFunction = board.placeSingleShip(ship, () => 5, () => 10, () => BoardDirection.East)
        val newBoardByDirection = board.placeSingleShip(ship, Coordinates(5,10), BoardDirection.East)
        val newBoardByCoordinates = board.placeSingleShip(ship2, ship2Coordinates)

        "be failure" in {
          assert(newBoardByFunction.isFailure)
          assert(newBoardByDirection.isFailure)
          assert(newBoardByCoordinates.isFailure)
        }
      }

      "col lower 0" should {
        val ship2Coordinates = Vector(Coordinates(5, -1), Coordinates(5, -2))
        val board = Board(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, Vector(ship2), Vector.empty)
        val newBoardByFunction = board.placeSingleShip(ship, () => 5, () => -1, () => BoardDirection.West)
        val newBoardByDirection = board.placeSingleShip(ship, Coordinates(5,-1), BoardDirection.West)
        val newBoardByCoordinates = board.placeSingleShip(ship2, ship2Coordinates)

        "be failure" in {
          assert(newBoardByFunction.isFailure)
          assert(newBoardByDirection.isFailure)
          assert(newBoardByCoordinates.isFailure)
        }
      }
    }
  }
}
