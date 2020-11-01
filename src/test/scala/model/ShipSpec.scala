package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ShipSpec extends AnyWordSpec with Matchers {
  "A Ship" when {
    "new" should {
      val ship = Ship(2, "Name")
      "have a length" in {
        ship.length should be(2)
      }
      "have a name" in {
        ship.name should be("Name")
      }
    }
  }
}
