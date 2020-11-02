package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardCellSpec extends AnyWordSpec with Matchers {
  "A BoardCell" when {
    "hit" should {
      val boardCell = BoardCell(true)

      "be hit" in {
        assert(boardCell.isHit == true)
      }
    }

    "not hit" should {
      val boardCell = BoardCell(false)

      "not be hit" in {
        assert(boardCell.isHit == false)
      }
    }
  }
}
