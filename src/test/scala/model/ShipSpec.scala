package model

import org.scalatest.wordspec.AnyWordSpec

class ShipSpec extends AnyWordSpec {
  "A Ship" when {
    "new" should {
      val length = 2
      val name = "Name"

      val ship = Ship(length, name)

      "have a length" in {
        assert(ship.length == length)
      }
      "have a name" in {
        assert(ship.name == name)
      }
    }
  }
}
