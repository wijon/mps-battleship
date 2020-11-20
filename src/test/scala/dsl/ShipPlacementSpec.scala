package dsl

import dataTransferObjects.{Coordinates, Ship}
import dsl.intern.ShipPlacement
import enums.{BoardDirection, Player1, Player2}
import org.scalatest.wordspec.AnyWordSpec

class ShipPlacementSpec extends AnyWordSpec {
  "A ship positioning DSL" when {

    "placing a ship for player 1" should {
      val shipPlacement = new ShipPlacement
      shipPlacement place "Submarine" length 3 at 14 facing BoardDirection.West as Player1

      "generate positioning data for player 1" in {
        assert(shipPlacement.shipPositionsPlayer1.size == 1)
        assert(shipPlacement.shipPositionsPlayer2.isEmpty)

        val shipPosition = shipPlacement.shipPositionsPlayer1.head
        assert(shipPosition.ship == Ship(3, "Submarine"))
        assert(shipPosition.positions == Vector(Coordinates(1, 4), Coordinates(1, 3), Coordinates(1, 2)))
      }
    }

    "placing a ship for player 2" should {
      val shipPlacement = new ShipPlacement
      shipPlacement place "Destroyer" length 2 at 44 facing BoardDirection.East as Player2

      "generate positioning data for player 2" in {
        assert(shipPlacement.shipPositionsPlayer1.isEmpty)
        assert(shipPlacement.shipPositionsPlayer2.size == 1)

        val shipPosition = shipPlacement.shipPositionsPlayer2.head
        assert(shipPosition.ship == Ship(2, "Destroyer"))
        assert(shipPosition.positions == Vector(Coordinates(4, 4), Coordinates(4, 5)))
      }
    }
  }
}
