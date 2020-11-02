
import model.{Board, BoardCell, Coordinates, OutputHelper, Ship, ShipPosition}

import scala.collection.immutable.HashMap

//case class Settings()
//abstract class Player(ownBoard: Board, enemyBoard: Board)
//case class HumanPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)
//abstract class AiPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)
//case class EasyAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)
//case class MediumAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)
//case class DifficultAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)







//object Game {
//  /** Board of human player */
//  //    var player1Board: Board = new Board(getShips())
//  //    /** Board of AI player */
//  //    var player2Board: Board = new Board(getShips())
//
//  /** Check if all Ships of one Player are destroyed
//   *
//   * @return game still running?
//   */
//  def isRunning(): Boolean = {
//    // TODO
//    throw new NotImplementedError()
//  }
//
//  /** Check if human player is winner
//   *
//   * @return Winner?
//   */
//  def isWinner(): Boolean = {
//    // TODO
//    // Was passiert, wenn Spiel noch läuft?!
//    throw new NotImplementedError()
//  }
//
//  /** Create ships
//   *
//   * @return List of all ships
//   */
//  def getShips(): Vector[Ship] = {
//    return Vector(
//      Ship(5, "Carrier"),
//      Ship(4, "Battleship"),
//      Ship(3, "Cruiser"),
//      Ship(3, "Submarine"),
//      Ship(2, "Destroyer")
//    )
//  }
//
//  /** Make a move for board
//   *
//   * @param player1     board of player1? (human player)
//   * @param coordinates coordinates to shoot
//   * @return Board after play
//   */
//  def shootAtBoard(player1: Boolean, coordinates: Coordinates): Board = {
//    // TODO
//    throw new NotImplementedError()
//  }
//}
//
//
//
//
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
//
//  /** Shoot at BoardCell
//   *
//   * @param row row coordinate
//   * @param col col coordinate
//   * @return a tuple containing the updated board together with a boolean reflecting if the shot hit a ship
//   */
//  def shoot(row: Int, col: Int): (Board, Boolean) = {
//    // TODO
//    // Wichtig: Was passiert, wenn Board Cell bereits beschossen wurde?!
//
//    // Update board cell in matrix
//    val newMatrix = matrix.updated(row, matrix(row).updated(col, BoardCell(true)))
//
//    // Check if a ship was hit
//    val shipPos = shipPositions.find(sp => sp.positions.contains(Coordinates(row, col)))
//
//    var isHit = false
//    if (shipPos.isDefined) {
//      // Ship was hit
//      isHit = true
//    }
//
//    // Return a copy of the current object with the updated matrix
//    return (copy(newMatrix, ships, shipPositions), isHit)
//  }
//
//  /** Checks if ship is destroyed
//   *
//   * @param ship Ship to check
//   * @return destroyed?
//   */
//  def isDestroyed(ship: Ship): Boolean = {
//    // Get position for given ship
//    val shipPosition = shipPositions.find(sp => sp.ship == ship)
//
//    // Check if there is a position given for the ship
//    if (shipPosition.isEmpty) {
//      // No position given, return false (ToDo: Handle differently?)
//      return false
//    }
//
//    // Check if every board cell within the ships position is hit
//    return shipPosition.get.positions.forall(c => {
//      matrix(c.row)(c.col).isHit
//    })
//  }
//}