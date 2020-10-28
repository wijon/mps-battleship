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
    // TODO
    throw new NotImplementedError()
  }

  /** Render loss-screen for output
   *
   * @return loss-screen
   */
  def generateLoss(): Vector[String] = {
    // TODO
    throw new NotImplementedError()
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