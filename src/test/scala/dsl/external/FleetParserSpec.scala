package dsl.external

import dsl.extern.{FleetParser, ShipMetaData}
import enums.{BoardDirection, Player}
import org.scalatest.wordspec.AnyWordSpec

class FleetParserSpec extends AnyWordSpec {
  "FleetParser" when {
    "parse correct fleet text including player 1 and player 2" should {
      val txt = "Player: 1\nName\t\tLength\tStartingPos\tDirection\nCarrier\t\t5\t14\t\tEast\nBattleship\t4\t23\t\tSouth\n\nPlayer: 2\nName\t\tLength\tStartingPos\tDirection\nCarrier\t\t5\t38\t\tWest\nBattleship\t4\t72\t\tNorth"
      val parser = new FleetParser
      val result = parser.parseFleetText(txt)

      "succeed" in {
        assert(result.isSuccess)
      }

      "return player 1 data" in {
        val player1 = result.get(0)

        assert(player1.player == Player.Player1)
        assert(player1.ships == Vector(
          ShipMetaData("Carrier", 5, 14, BoardDirection.East),
          ShipMetaData("Battleship", 4, 23, BoardDirection.South)
        ))
      }

      "return player 2 data" in {
        val player2 = result.get(1)

        assert(player2.player == Player.Player2)
        assert(player2.ships == Vector(
          ShipMetaData("Carrier", 5, 38, BoardDirection.West),
          ShipMetaData("Battleship", 4, 72, BoardDirection.North)
        ))
      }
    }

//    "parse incorrect fleet text" should {
//      val txt = "Player: 1\nNome\t\tLength\tStartingPos\tDirection\nCarrier\t\ts\t14\t\tEast\nBattleship\t4\t23\t\tSouth\n\nPlayer: 2\nName\t\tLength\tStartingPos\tDirection\nCarrier\t\t5\t38\t\tWest\nBattleship\t4\t72\t\tNorth"
//      val parser = new FleetParser
//      val result = parser.parseFleetText(txt)
//      val x = result;
//
//      "fail" in {
//        assert(!result.isSuccess)
//      }
//    }
  }
}
