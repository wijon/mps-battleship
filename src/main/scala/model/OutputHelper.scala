package model

/** Helper functions for output rendering
 *
 */
object OutputHelper {
  /** Render board for output
   *
   * @param headline  Headline for Board
   * @param board     Board to render
   * @param showShips Show ships on board?
   * @return
   */
  def generateBoard(headline: String, board: Board, showShips: Boolean): Vector[String] = {
    // TODO
    // Idee: Hit = X, Ship = O, HitShip = ∅

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

    return Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
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

    return Vector(viewLine1, viewLine2, viewLine3, viewLine4, viewLine5)
  }

  /** Render remaining ships-Infotext for output
   *
   * @param board Board with remaining ships
   * @return Remaining ships infotext
   */
  def generateRemainingShips(board: Board): Vector[String] = {
    // TODO
    throw new NotImplementedError()
  }

  /** Render round info text for output
   *
   * @return Round info text
   */
  def generateRoundInfoText(): Vector[String] = {
    // TODO
    // Idee: Rundennummer?, Überschrift (Spielername, ...)?
    throw new NotImplementedError()
  }
}