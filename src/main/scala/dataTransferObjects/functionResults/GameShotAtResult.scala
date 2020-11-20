package dataTransferObjects.functionResults

import dataTransferObjects.ShipPosition
import model.Game

case class GameShotAtResult(game: Game, shipPosition: Option[ShipPosition])