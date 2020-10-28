case class Settings()

case class Ship(id: Int, length: Int, destroyed: Boolean, counterHit: Int)

case class BoardField(shipId: Int, hit: Boolean)

case class Board(matrix: Vector[Vector[BoardField]], ships: Vector[Ship])

abstract class Player(ownBoard: Board, enemyBoard: Board)

case class HumanPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)

abstract class AiPlayer(ownBoard: Board, enemyBoard: Board) extends Player(ownBoard, enemyBoard)

case class EasyAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)

case class MediumAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)

case class DifficultAiPlayer(ownBoard: Board, enemyBoard: Board) extends AiPlayer(ownBoard, enemyBoard)

case class Game(player1: Player, player2: Player)






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