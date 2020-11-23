package dsl.extern

import enums.BoardDirection.BoardDirection

case class ShipMetaData(name: String, length: Int, startingPos: Int, dir: BoardDirection)
