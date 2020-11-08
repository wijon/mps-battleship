
import model.{Board, BoardCell, Coordinates, OutputHelper, Ship, ShipPosition}

import scala.collection.immutable.HashMap






























//case class Settings()
//abstract class Player(ownBoard: Board, enemyBoard: Board)
//case class HumanPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)
//abstract class AiPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)
//case class EasyAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)
//case class MediumAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)
//case class DifficultAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)




//case class Board(matrix: Vector[Vector[BoardCell]], ships: Vector[Ship], shipPositions: Vector[ShipPosition]) {
//  // Override constructor, this one is used for the initial instantiation
//  def this(ships: Vector[Ship]) = this(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, ships, Vector())
//
//  /** Places all ships in matrix
//   *
//   * @return matrix with ships
//   */
//  def placeShips(): HashMap[Coordinates, BoardCell] = {
//    // TODO
//    // Wichtig: Random Generator als Übergabeparameter verwenden!
//    throw new NotImplementedError()
//  }
//}