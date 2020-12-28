package dsl.extern

import enums.Player

case class FleetParserResult(player: Player, ships: Vector[ShipMetaData])
