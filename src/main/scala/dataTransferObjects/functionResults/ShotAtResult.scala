package dataTransferObjects.functionResults

import dataTransferObjects.ShipPosition
import model.Board

case class ShotAtResult(board: Board, shipPosition: Option[ShipPosition])
