package dsl.extern

import enums.Player.Player

case class FleetParserResult(player: Player, ships: Vector[ShipMetaData])
