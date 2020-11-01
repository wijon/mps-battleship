package model

import org.scalatest.wordspec.AnyWordSpec

class ShipSpec extends AnyWordSpec {
  "A Ship" when {
    "new" should {
      val ship = Ship(2, "Name")
      "have a length" in {
        assert(ship.length == 2)
      }
      "have a name" in {
        assert(ship.name == "Name")
      }
    }
  }
}
