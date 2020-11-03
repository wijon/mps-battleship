
import model.{Board, BoardCell, Coordinates, OutputHelper, Ship, ShipPosition}

import scala.collection.immutable.HashMap





val ships = Vector(
  Ship(2, "Test 1"),
  Ship(3, "Test 2"),
  Ship(5, "Test 2"),
)
val matrix = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
val shipPositions = Vector(
  ShipPosition(ships(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
  ShipPosition(ships(1), Vector(Coordinates(1,1), Coordinates(1,2), Coordinates(1,3))),
  ShipPosition(ships(2), Vector(Coordinates(9,0), Coordinates(8,0), Coordinates(7,0), Coordinates(6,0), Coordinates(5,0))),
)

val matrix1 = matrix.updated(3, matrix(3).updated(4, BoardCell(true)))
val matrix2 = matrix1.updated(3, matrix1(3).updated(5, BoardCell(true)))
val matrix3 = matrix2.updated(1, matrix2(1).updated(2, BoardCell(true)))
val matrix4 = matrix3.updated(9, matrix3(9).updated(8, BoardCell(true)))
val matrix5 = matrix4.updated(7, matrix4(7).updated(6, BoardCell(true)))
val testBoard = Board(matrix5, ships, shipPositions)

val test = OutputHelper.generateBoard(testBoard, true)
test.foreach(l => println(l))






val shipsB = Vector(
  Ship(2, "Test 1"),
  Ship(3, "Test 2")
)
val matrixB = Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }
val shipPositionsB = Vector(
  ShipPosition(shipsB(0), Vector(Coordinates(3, 4), Coordinates(3, 5))),
  ShipPosition(shipsB(1), Vector(Coordinates(1,1), Coordinates(1,2), Coordinates(1,3)))
)

val matrixB1 = matrixB.updated(3, matrixB(3).updated(4, BoardCell(true)))
val matrixB2 = matrixB1.updated(3, matrixB1(3).updated(5, BoardCell(true)))
val matrixB3 = matrixB2.updated(1, matrixB2(1).updated(2, BoardCell(true)))
val testBoardB = Board(matrixB3, shipsB, shipPositionsB)

val test2 = OutputHelper.generateRemainingShips(testBoardB)
test2.foreach(l => println(l))




































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