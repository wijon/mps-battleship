import model.Game
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BattleshipSpec extends AnyWordSpec with Matchers {
  "Battleship" when {
    "getShips() is called" should {
      val ships = Battleship.getShips()
      "return a total of 5 ships" in {
        ships.length should be(5)
      }
      "contain a Carrier with length 5" in {
        ships.exists(s => s.name == "Carrier" && s.length == 5) should be(true)
      }
      "contain a Battleship with length 4" in {
        ships.exists(s => s.name == "Battleship" && s.length == 4) should be(true)
      }
      "contain a Cruiser with length 3" in {
        ships.exists(s => s.name == "Cruiser" && s.length == 3) should be(true)
      }
      "contain a Submarine with length 3" in {
        ships.exists(s => s.name == "Submarine" && s.length == 3) should be(true)
      }
      "contain a Destroyer with length 2" in {
        ships.exists(s => s.name == "Destroyer" && s.length == 2) should be(true)
      }
    }
  }
}
