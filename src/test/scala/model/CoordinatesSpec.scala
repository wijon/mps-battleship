package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CoordinatesSpec extends AnyWordSpec with Matchers {
  "Coordinates" when {
    "created" should {
      val coordinate = Coordinates(3, 10)

      "have an X-coordinate" in {
        assert(!coordinate.row.isNaN)
      }

      "have a Y-coordinate" in {
        assert(!coordinate.col.isNaN)
      }
    }
  }
}
