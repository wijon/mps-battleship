package model

import org.scalatest.wordspec.AnyWordSpec

class ShipPositionSpec extends AnyWordSpec {
  "A ShipPosition" when {
    "new" should {
      val ship = Ship(2, "Test")
      val positions = Vector(Coordinates(1,1), Coordinates(1,2))
      val shipPosition = ShipPosition(ship, positions)

      "have a ship" in {
        assert(shipPosition.ship == ship)
      }

      "have positions" in {
        assert(shipPosition.positions == positions)
      }
    }
  }
}
