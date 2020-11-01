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
      val result2Shot = result1Shot._1.shoot(8, 1)
      "register BoardCell hit" in {
        assert(result1Shot._1.matrix(3)(4).isHit)
        assert(result2Shot._1.matrix(8)(1).isHit)
      }
      "register no ship hit" in {
        assert(!result2Shot._2)
      }
      "register ship hit" in {
        assert(result1Shot._2)
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
        assert(board.isDestroyed(ships(0)))
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
        assert(!board.isDestroyed(ships(0)))
      }
    }
  }
}
