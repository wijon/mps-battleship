package view

import dataTransferObjects.{Coordinates, Ship}
import model.{Board, Game}

import scala.util.Try

/** Helper functions for output rendering
 *
 */
object OutputHelper {
  /** Render board for output
   *
   * @param board     Board to render
   * @param showShips Show ships on board?
   * @param player    Player name
   * @return
   */
  def generateBoard(board: Board, showShips: Boolean, player: String): Vector[String] = {
    val headline = "Spielbrett " + player
    val line1 = "~~~~~~~~~~~~~"
    val line2 = "~ 0123456789~"
    val line13 = "~~~~~~~~~~~~~"
    Vector(headline, line1, line2) ++ generateBoardLineByLine(0, board, showShips) :+ line13
  }

  /** Render board line by line
   *
   * @param currentRow Index of current row
   * @param board      Board to render
   * @param showShips  Show ships in board?
   * @return Board for output
   */
  private def generateBoardLineByLine(currentRow: Int,
                                      board: Board,
                                      showShips: Boolean): Vector[String] = {
    currentRow match {
      case x if x >= 10 => Vector.empty
      case _ =>
        val newLine = "~" + currentRow + generateBoardLineFieldByField(currentRow, 0, board, showShips) + "~"
        Vector(newLine) ++ generateBoardLineByLine(currentRow + 1, board, showShips)
    }
  }

  /** Render board line field by field
   *
   * @param currentRow Index of current row
   * @param currentCol Index of current column
   * @param board      Board to render
   * @param showShips  Show ships in board?
   * @return BoardRow for output
   */
  private def generateBoardLineFieldByField(currentRow: Int,
                                            currentCol: Int,
                                            board: Board,
                                            showShips: Boolean): String = {
    currentCol match {
      case x if x >= 10 => ""
      case _ =>
        val cellIsHit = board.matrix(currentRow)(currentCol).isHit
        val shipPos = board.shipPositions.find(_.positions.contains(Coordinates(currentRow, currentCol)))

        val newOutput = if (cellIsHit) {
          if (shipPos.isDefined) "@" else "X"
        } else {
          if (shipPos.isDefined && showShips) "O" else " "
        }

        newOutput + generateBoardLineFieldByField(currentRow, currentCol + 1, board, showShips)
    }
  }

  /** Render remaining ships-Infotext for output
   *
   * @param board  Board with remaining ships
   * @param player Player name
   * @return Remaining ships infotext
   */
  def generateRemainingShips(board: Board, player: String): Vector[String] = {
    val headline = "Schiffstatus " + player
    Vector(headline) ++ generateRemainingShipsLineByLine(board.shipPositions.map(_.ship), board)
  }

  /** Render remaining ships-infotext ship by ship
   *
   * @param remainingShips Remaining ships to process
   * @param board          Board to process
   * @return Remaining ships infotext
   */
  private def generateRemainingShipsLineByLine(remainingShips: Vector[Ship], board: Board): Vector[String] = {
    remainingShips match {
      case x if x.isEmpty => Vector.empty
      case _ =>
        val shipToProcess = remainingShips(0)
        val spacesToAdd = 11 - shipToProcess.name.length
        val newLine = shipToProcess.name + (" " * spacesToAdd) + generateShipHits(shipToProcess, board)
        Vector(newLine) ++ generateRemainingShipsLineByLine(remainingShips.drop(1), board)
    }
  }

  /** Render info text for single ship
   *
   * @param ship  Ship to process
   * @param board Board to process
   * @return Info text for single ship
   */
  private def generateShipHits(ship: Ship, board: Board): String = {
    val shipPos = board.shipPositions.find(_.ship == ship)
    val singleShipData = generateSingleShipElement(shipPos.get.positions, board, 0)
    val shipAsVisual = "\\" + singleShipData._1 + "/" + (" " * (6 - ship.length))
    val hitInfoText = singleShipData._2 + " hit(s)"
    val destroyedInfoText = if (singleShipData._2 == ship.length) ", destroyed" else ""
    shipAsVisual + hitInfoText + destroyedInfoText
  }

  /** Render info text for single ship field by field
   *
   * @param remainingShipPos Remaining ship positions to process
   * @param board            Board to process
   * @return Single ship info text and number of hits
   */
  private def generateSingleShipElement(remainingShipPos: Vector[Coordinates],
                                        board: Board,
                                        currentNumberOfHits: Int): (String, Int) = {
    if (remainingShipPos.isEmpty) {
      ("", currentNumberOfHits)
    } else {
      val shipPositionToProcess = remainingShipPos(0)
      val boardCellHit = board.matrix(shipPositionToProcess.row)(shipPositionToProcess.col).isHit
      val newChar = if (boardCellHit) 'X' else '_'
      val newNumberOfHits = if (boardCellHit) currentNumberOfHits + 1 else currentNumberOfHits

      val recurseValue = generateSingleShipElement(remainingShipPos.drop(1), board, newNumberOfHits)
      (newChar + recurseValue._1, recurseValue._2)
    }
  }

  /** Render final game info for output
   *
   * @param game Game to check
   * @return Victory / Loss-Screen
   */
  def generateFinalText(game: Game): Try[Vector[String]] = {
    Try(if (game.isRunning)
      throw new IllegalStateException
    else if (game.humanPlayerIsWinner()) generateVictory() else generateLoss()
    )
  }

  /** Render victory-screen for output
   *
   * @return Victory-screen
   */
  def generateVictory(): Vector[String] = {
    val viewLine1 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    val viewLine2 = "XXXX                           XXXX"
    val viewLine3 = "XXXX   SIE HABEN GEWONNEN :)   XXXX"
    val viewLine4 = "XXXX                           XXXX"
    val viewLine5 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
  }

  /** Render loss-screen for output
   *
   * @return loss-screen
   */
  def generateLoss(): Vector[String] = {
    val viewLine1 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    val viewLine2 = "XXXX                           XXXX"
    val viewLine3 = "XXXX   SIE HABEN VERLOREN :(   XXXX"
    val viewLine4 = "XXXX                           XXXX"
    val viewLine5 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
  }

  /** Generate round text for output
   *
   * @param game Game
   * @return Round text for output
   */
  def generateRoundText(game: Game): Vector[String] = {
    val roundInfoText = generateRoundInfoText(game)
    val shipsInfoText = generateRemainingShips(game.humanPlayerBoard, "Mensch")
    val humanBoard = generateBoard(game.humanPlayerBoard, showShips = true, "Mensch")
    val aiBoard = generateBoard(game.aiPlayerBoard, showShips = true, "KI")

    val maxLength = humanBoard.map(str => str.length).max
    val boardText = (humanBoard zip aiBoard).map(x => s"${x._1.padTo(maxLength, ' ')}\t\t\t${x._2}")

    Vector(" ") ++ roundInfoText ++ Vector(" ") ++ shipsInfoText ++ Vector(" ") ++ boardText
  }

  /** Render round info text for output
   *
   * @param game Current game
   * @return Round info text
   */
  def generateRoundInfoText(game: Game): Vector[String] = {
    val viewLine1 = "Runde " + game.roundNum
    Vector(viewLine1)
  }

  /** Render info text for output
   *
   * @return Info text human players turn
   */
  def generateHumanPlayerRoundInfoText(): Vector[String] = {
    val viewLine1 = "Sie sind am Zug."
    val viewLine2 = "Welches Feld möchten Sie beschießen?"
    Vector(viewLine1, viewLine2)
  }

  /** Render info text for output
   *
   * @return Info text ai players turn
   */
  def generateAiPlayerRoundInfoText(): Vector[String] = {
    val viewLine1 = "Der Computerspieler ist am Zug."
    Vector(viewLine1)
  }

  /** Render info text about shoot for output
   *
   * @param row Shot row
   * @param col Shot column
   * @return Shoot info text
   */
  def generateShootInfoText(row: Int, col: Int): Vector[String] = {
    val viewLine1 = "Beschuss auf Feld " + row.toString + col.toString
    Vector(viewLine1)
  }

  /** Render info text about ship hit for output
   *
   * @param ship        Ship
   * @param isDestroyed Is Ship destroyed?
   * @return Infotext for output
   */
  def generateShipHitInfoText(ship: Ship, isDestroyed: Boolean): Vector[String] = {
    val viewLine1 = "Das Schiff " + ship.name + " wurde getroffen."

    if (isDestroyed)
      Vector(viewLine1) ++ generateShipDestroyedInfoText(ship)
    else
      Vector(viewLine1)
  }

  /** Render info text about destroyed ship for output
   *
   * @param ship Ship
   * @return Infotext for output
   */
  def generateShipDestroyedInfoText(ship: Ship): Vector[String] = {
    val viewLine1 = "Das Schiff " + ship.name + " wurde versenkt."
    Vector(viewLine1)
  }

  /** Render info text about nothing hit for output
   *
   * @return Infotext for output
   */
  def generateNothingHitInfoText(): Vector[String] = {
    val viewLine1 = "Es wurde nichts getroffen."
    Vector(viewLine1)
  }

  /** Render info text that player is allowed to shoot again
   *
   * @return Infotext for output
   */
  def generateShootAgainInfoText(): Vector[String] = {
    val viewLine1 = "Spieler ist erneut am Zug."
    Vector(viewLine1)
  }

  /** Render info text that row is not a number
   *
   * @return Infotext for output
   */
  def generateInvalidRowInputInfoText(): Vector[String] = {
    val viewLine1 = "Zeilenwert unzulässig. Der Wert muss zwischen 0 und 9 liegen."
    val viewLine2 = "Bitte geben Sie einen neuen Wert ein."
    Vector(viewLine1, viewLine2)
  }

  /** Render info text that column is not a number
   *
   * @return Infotext for output
   */
  def generateInvalidColInputInfoText(): Vector[String] = {
    val viewLine1 = "Spaltenwert unzulässig. Der Wert muss zwischen 0 und 9 liegen."
    val viewLine2 = "Bitte geben Sie einen neuen Wert ein."
    Vector(viewLine1, viewLine2)
  }

  /** Render info text that coordinates where already hit
   *
   * @return Infotext for output
   */
  def generateInvalidInputInfoText(): Vector[String] = {
    val viewLine1 = "Eingabe unzulässig. Das Feld wurde bereits beschossen."
    val viewLine2 = "Bitte geben Sie einen neuen Wert ein."
    Vector(viewLine1, viewLine2)
  }
}
