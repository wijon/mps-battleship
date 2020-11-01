package model

import org.scalatest.wordspec.AnyWordSpec

class GameSpec extends AnyWordSpec {
  "Game" when {
    "getShips() is called" should {
      val ships = Game.getShips()
      "return a total of 5 ships" in {
        assert(ships.length == 5)
      }
      "contain a Carrier with length 5" in {
        assert(ships.exists(s => s.name == "Carrier" && s.length == 5))
      }
      "contain a Battleship with length 4" in {
        assert(ships.exists(s => s.name == "Battleship" && s.length == 4))
      }
      "contain a Cruiser with length 3" in {
        assert(ships.exists(s => s.name == "Cruiser" && s.length == 3))
      }
      "contain a Submarine with length 3" in {
        assert(ships.exists(s => s.name == "Submarine" && s.length == 3))
      }
      "contain a Destroyer with length 2" in {
        assert(ships.exists(s => s.name == "Destroyer" && s.length == 2))
      }
    }
  }
}
