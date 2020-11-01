package model

import org.scalatest.wordspec.AnyWordSpec

class OutputHelperSpec extends AnyWordSpec {
  "Victory view" when {
    "shown" should {
      "not be empty" in {
        assert(!OutputHelper.generateVictory().isEmpty)
      }

      "contain victory text" in {
        assert(OutputHelper.generateVictory().mkString(" ").contains("SIE HABEN GEWONNEN"))
      }

      "contain a happy face" in {
        assert(OutputHelper.generateVictory().mkString(" ").contains(":)"))
      }
    }
  }

  "Loss view" when {
    "shown" should {
      "not be empty" in {
        assert(!OutputHelper.generateLoss().isEmpty)
      }

      "contain victory text" in {
        assert(OutputHelper.generateLoss().mkString(" ").contains("SIE HABEN VERLOREN"))
      }

      "contain a sad face" in {
        assert(OutputHelper.generateLoss().mkString(" ").contains(":("))
      }
    }
  }
}
