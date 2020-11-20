package dataTransferObjects

import org.scalatest.wordspec.AnyWordSpec

class CoordinatesSpec extends AnyWordSpec {
  "Coordinates" when {
    "extracted from string" should {
      "extract correct coordinates" in {
        val rowInput = 4
        val colInput = 6

        val input = rowInput.toString + colInput.toString
        val coordinates = Coordinates.coordinatesFromString(input)

        assert(coordinates.isSuccess)
        assert(rowInput == coordinates.get.row)
        assert(colInput == coordinates.get.col)
      }

      "fail when row is not a digit" in {
        val rowInput = "A"
        val colInput = 6

        val input = rowInput + colInput.toString
        val coordinates = Coordinates.coordinatesFromString(input)

        assert(coordinates.isFailure)
        assert(coordinates.failed.get.getMessage == "row")
      }

      "fail when column is not a digit" in {
        val rowInput = 4
        val colInput = "A"

        val input = rowInput.toString + colInput
        val coordinates = Coordinates.coordinatesFromString(input)

        assert(coordinates.isFailure)
        assert(coordinates.failed.get.getMessage == "col")
      }
    }
  }
}
