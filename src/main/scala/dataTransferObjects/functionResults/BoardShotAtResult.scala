package dataTransferObjects.functionResults

import dataTransferObjects.ShipPosition
import model.Board

case class BoardShotAtResult(board: Board, shipPosition: Option[ShipPosition])
