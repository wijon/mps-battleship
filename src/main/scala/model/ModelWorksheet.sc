import model.{Board, Coordinates, Ship}

case class Settings()

abstract class Player(ownBoard: Board, enemyBoard: Board)

case class HumanPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)

abstract class AiPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)

case class EasyAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)

case class MediumAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)

case class DifficultAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)







/** Get Main Menu View
 *
 * @return Main Menu-View as String to print
 */
def getMainMenuView(): Vector[String] = {
  val viewLine1 = ">>> Hauptmenü <<<"
  val viewLine2 = "(1) Spieler gegen Spieler"
  val viewLine3 = "(2) Spieler gegen Computer"
  val viewLine4 = "(3) Computer gegen Computer"
  val viewLine5 = "(4) Einstellungen"
  return Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
}

/** Get Settings-View
 *
 * @return Settings-View as String to print
 */
def getSettingsView(): Vector[String] = {
  val viewLine1 = ">>> Einstellungen <<<"
  val viewLine2 = "Bisher gibt es noch keine Einstellungsmöglichkeiten."
  return Vector(viewLine1, viewLine2)
}

/** Get PlayerVsPlayer-View
 *
 * @return PlayerVsPlayer-View as String to print
 */
def getPlayerVsPlayerView(): Vector[String] = {
  val viewLine1 = ">>> Spieler VS. Spieler <<<"
  val viewLine2 = "Geben Sie einen Namen für den 1. Spieler ein."
  return Vector(viewLine1, viewLine2)
}

/** Get PlayerVsAi-View
 *
 * @return PlayerVsAi-View as String to print
 */
def getPlayerVsAiView(): Vector[String] = {
  val viewLine1 = ">>> Spieler VS. Computer <<<"
  val viewLine2 = "Wählen Sie einen Schwierigkeitsgrad"
  val viewLine3 = "(1) Einfach"
  val viewLine4 = "(2) Mittel"
  val viewLine5 = "(3) Schwer"
  return Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
}

/** Get PlayerVsAi-View
 *
 * @return PlayerVsAi-View as String to print
 */
def getAiVsAiView(): Vector[String] = {
  val viewLine1 = ">>> Computer VS. Computer <<<"
  val viewLine2 = "Wählen Sie einen Schwierigkeitsgrad für Spieler 1"
  val viewLine3 = "(1) Einfach"
  val viewLine4 = "(2) Mittel"
  val viewLine5 = "(3) Schwer"
  return Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
}

/** Navigate through main menu
 *
 * @param menuItem Index of selected menu item
 * @return New View as String to print
 */
def navigationMainMenu(menuItem: Int): Vector[String] = {
  val navigatedView = menuItem match {
    case 1 => getPlayerVsPlayerView()
    case 2 => getPlayerVsAiView()
    case 3 => getAiVsAiView()
    case 4 => getSettingsView()
    case _ => getMainMenuView()
  }
  return navigatedView
}















/** Print Main Menu. Wait for Input. Navigate
 *
 */
def mainMenuInputSimulator(): Int = {
  val r = scala.util.Random
  r.nextInt(2) + 1
}

val mainMenuLines = getMainMenuView()
mainMenuLines.foreach(line => println(line))

Thread.sleep(3000)
val menuItem: Int = mainMenuInputSimulator()
val viewLines = navigationMainMenu(menuItem)
viewLines.foreach(line => println(line))









/** Render board for output
 *
 * @param board     Board to render
 * @param showShips Show ships on board?
 * @return
 */
def generateBoard(board: Board, showShips: Boolean): Vector[String] = {
  // TODO
  // Idee: Hit = X, Ship = O, HitShip = ∅
  // Überschrift? Spielerbrett / Gegnerbrett?

  val line1 = "~~~~~~~~~~~~~"
  val line2 = "~ 0123456789~"
  val line3 = "~0          ~"
  val line4 = "~1          ~"
  val line5 = "~2          ~"
  val line6 = "~3          ~"
  val line7 = "~4          ~"
  val line8 = "~5          ~"
  val line9 = "~6          ~"
  val line10 = "~7          ~"
  val line11 = "~8          ~"
  val line12 = "~9          ~"
  val line13 = "~~~~~~~~~~~~~"

  return Vector(line1, line2, line3, line4, line5, line6,
    line7, line8, line9, line10, line11, line12, line13)
}


/** Render remaining ships-Infotext for output
 *
 * @param board Board with remaining ships
 * @return Remaining ships infotext
 */
def generateRemainingShips(board: Board): Vector[String] = {
  // TODO

  val viewLine1 = "U-Boot         \\__/    0 hits"
  val viewLine2 = "Zerstörer 1    \\__X/   1 hit"
  val viewLine3 = "Zerstörer 2    \\XXX/   3 hits, versenkt"
  val viewLine4 = "Kreuzer        \\____/  0 hits"
  val viewLine5 = "Schlachtschiff \\_X_XX/ 3 hits"

  return Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
}










object Game {
  /** Board of human player */
  //    var player1Board: Board = new Board(getShips())
  //    /** Board of AI player */
  //    var player2Board: Board = new Board(getShips())

  /** Check if all Ships of one Player are destroyed
   *
   * @return game still running?
   */
  def isRunning(): Boolean = {
    // TODO
    throw new NotImplementedError()
  }

  /** Check if human player is winner
   *
   * @return Winner?
   */
  def isWinner(): Boolean = {
    // TODO
    // Was passiert, wenn Spiel noch läuft?!
    throw new NotImplementedError()
  }

  /** Create ships
   *
   * @return List of all ships
   */
  def getShips(): Vector[Ship] = {
    return Vector(
      Ship(5, "Carrier"),
      Ship(4, "Battleship"),
      Ship(3, "Cruiser"),
      Ship(3, "Submarine"),
      Ship(2, "Destroyer")
    )
  }

  /** Make a move for board
   *
   * @param player1     board of player1? (human player)
   * @param coordinates coordinates to shoot
   * @return Board after play
   */
  def shootAtBoard(player1: Boolean, coordinates: Coordinates): Board = {
    // TODO
    throw new NotImplementedError()
  }
}




case class Board(matrix: Vector[Vector[BoardCell]], ships: Vector[Ship], shipPositions: Vector[ShipPosition]) {
  // Override constructor, this one is used for the initial instantiation
  def this(ships: Vector[Ship]) = this(Vector.tabulate(10, 10) { (_, _) => BoardCell(false) }, ships, Vector())

  /** Places all ships in matrix
   *
   * @return matrix with ships
   */
  def placeShips(): HashMap[Coordinates, BoardCell] = {
    // TODO
    // Wichtig: Random Generator als Übergabeparameter verwenden!
    throw new NotImplementedError()
  }

  /** Shoot at BoardCell
   *
   * @param row row coordinate
   * @param col col coordinate
   * @return a tuple containing the updated board together with a boolean reflecting if the shot hit a ship
   */
  def shoot(row: Int, col: Int): (Board, Boolean) = {
    // TODO
    // Wichtig: Was passiert, wenn Board Cell bereits beschossen wurde?!

    // Update board cell in matrix
    val newMatrix = matrix.updated(row, matrix(row).updated(col, BoardCell(true)))

    // Check if a ship was hit
    val shipPos = shipPositions.find(sp => sp.positions.contains(Coordinates(row, col)))

    var isHit = false
    if (shipPos.isDefined) {
      // Ship was hit
      isHit = true
    }

    // Return a copy of the current object with the updated matrix
    return (copy(newMatrix, ships, shipPositions), isHit)
  }

  /** Checks if ship is destroyed
   *
   * @param ship Ship to check
   * @return destroyed?
   */
  def isDestroyed(ship: Ship): Boolean = {
    // Get position for given ship
    val shipPosition = shipPositions.find(sp => sp.ship == ship)

    // Check if there is a position given for the ship
    if (shipPosition.isEmpty) {
      // No position given, return false (ToDo: Handle differently?)
      return false
    }

    // Check if every board cell within the ships position is hit
    return shipPosition.get.positions.forall(c => {
      matrix(c.row)(c.col).isHit
    })
  }
}