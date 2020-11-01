package model

import org.scalatest.wordspec.AnyWordSpec

class OutputHelperSpec extends AnyWordSpec {
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
      val textAsString = OutputHelper.generateRoundInfoText(roundNumber).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain round number" in {
        val regex = "^.*(Rundennummer: " + roundNumber + ").*$"
        assert(textAsString.matches(regex))
      }
    }
  }

  "Ai info text" when {
    "called" should {
      val testCoordinates = new Coordinates(3, 7)
      val textAsString = OutputHelper.generateAiInfoText(testCoordinates).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain the coordinates" in {
        val regex = "^.*" + testCoordinates.x + testCoordinates.y + ".*$"
        assert(textAsString.matches(regex))
      }
    }
  }

  "Ship hit info text" when {
    "called" should {
      val testShip = new Ship(3, "TestShip")
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
      val testShip = new Ship(3, "TestShip")
      val textAsString = OutputHelper.generateShipDestroyedInfoText(testShip).mkString(" ")

      "not be empty" in {
        assert(!textAsString.isEmpty)
      }

      "contain ship name" in {
        assert(textAsString.contains(testShip.name))
      }
    }
  }
}
