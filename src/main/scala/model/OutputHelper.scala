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
    throw new NotImplementedError()
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