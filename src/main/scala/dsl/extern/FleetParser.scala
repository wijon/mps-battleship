package dsl.extern

import enums.BoardDirection.BoardDirection
import enums.Player.Player
import enums.{BoardDirection, Player}

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

class FleetParser extends RegexParsers {
  def text: Parser[String] = """[a-zA-Z:]+""".r ^^ (_.trim)

  def integer: Parser[Int] = """\d+""".r ^^ (_.toInt)

  def intCoordinates: Parser[Int] = """\d\d""".r ^^ (_.toInt)

  def boardDirection: Parser[BoardDirection] = """(North|East|South|West)""".r ^^ BoardDirection.withName

  def player: Parser[Player] = text ~ integer ^^ {
    case _ ~ i => i match {
      case 1 => Player.Player1
      case 2 => Player.Player2
    }
  }

  def ship: Parser[ShipMetaData] = text ~ integer ~ intCoordinates ~ boardDirection ^^ {
    case name ~ len ~ coord ~ dir => ShipMetaData(name, len, coord, dir)
  }

  def ships: Parser[Vector[ShipMetaData]] = rep(ship) ^^ (ships => ships.toVector)

  def fleetParser: Parser[FleetParserResult] =
    player ~
      """Name[ \t]*Length[ \t]*StartingPos[ \t]*Direction""".r ~
      ships ^^ {
      case player ~ _ ~ ships => FleetParserResult(player, ships)
    }

  def multiFleetParser: Parser[Vector[FleetParserResult]] = repN(2, fleetParser) ^^ (fleets => fleets.toVector)

  def parseFleetText(txt: String) : Try[Vector[FleetParserResult]] = {
    parse(multiFleetParser, txt) match {
      case Success(matched, _) => scala.util.Success(matched)
      case Failure(msg, _) => scala.util.Failure(new Exception(s"FAILURE: $msg"))
    }
  }
}
