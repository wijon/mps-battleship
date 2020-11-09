package model

/** Helper functions for output rendering
 *
 */
object OutputHelper {
  /** Render board for output
   *
   * @param board     Board to render
   * @param showShips Show ships on board?
   * @return
   */
  def generateBoard(board: Board, showShips: Boolean): Vector[String] = {
    val line1 = "~~~~~~~~~~~~~"
    val line2 = "~ 0123456789~"
    val line13 = "~~~~~~~~~~~~~"
    Vector(line1, line2) ++ generateBoardLineByLine(0, board, showShips) :+ line13
  }

  /** Render board line by line
   *
   * @param currentRow    Index of current row
   * @param board         Board to render
   * @param showShips     Show ships in board?
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
   * @param currentRow    Index of current row
   * @param currentCol    Index of current column
   * @param board         Board to render
   * @param showShips     Show ships in board?
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
   * @param board Board with remaining ships
   * @return Remaining ships infotext
   */
  def generateRemainingShips(board: Board): Vector[String] = {
    generateRemainingShipsLineByLine(board.ships, board)
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

  /** Render round info text for output
   *
   * @param game Current game
   * @return Round info text
   */
  def generateRoundInfoText(game: Game): Vector[String] = {
    val viewLine1 = "Runde " + game.roundNum
    Vector(viewLine1)
  }

  /** Render info text about AI-actions for output
   *
   * @param coordinates Coordinates that were shot at
   * @return AI info text
   */
  def generateAiInfoText(coordinates: Coordinates): Vector[String] = {
    val viewLine1 = "Der Computerspieler greift das Feld " + coordinates.row + coordinates.col + " an."
    Vector(viewLine1)
  }

  /** Render info text about ship hit for output
   *
   * @param ship Ship
   * @return Text
   */
  def generateShipHitInfotext(ship: Ship): Vector[String] = {
    val viewLine1 = "Das Schiff " + ship.name + " wurde getroffen."
    Vector(viewLine1)
  }

  /** Render info text about destroyed ship for output
   *
   * @param ship Ship
   * @return Text
   */
  def generateShipDestroyedInfoText(ship: Ship): Vector[String] = {
    val viewLine1 = "Das Schiff " + ship.name + " wurde versenkt."
    Vector(viewLine1)
  }
}