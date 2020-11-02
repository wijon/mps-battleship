package model

import org.scalatest.wordspec.AnyWordSpec

class GameSpec extends AnyWordSpec {
  "A Game" when {
    "new" should {
      val p1Ships = Vector(
        Ship(1, "A")
      )
      val p2Ships = Vector(
        Ship(2, "B")
      )
      val game = new Game(p1Ships, p2Ships)
      "have player 1 board with player 1 ships" in {
        assert(game.player1Board.ships == p1Ships)
      }
      "have player 2 board with player 2 ships" in {
        assert(game.player2Board.ships == p2Ships)
      }
    }
  }
}
