package model

/** Helper functions for output rendering
 *
 */
object OutputHelper {
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

  /** Render round info text for output
   *
   * @return Round info text
   */
  def generateRoundInfoText(roundNumber: Int): Vector[String] = {
    // TODO
    // Idee: Rundennummer?, Überschrift (Spielername, ...)?

    val viewLine1 = "Sie sind am Zug. Rundennummer: " + roundNumber
    return Vector(viewLine1)
  }

  /** Render info text about AI-actions for output
   *
   * @param coordinates Coordinates that were shot at
   * @return AI info text
   */
  def generateAiInfoText(coordinates: Coordinates): Vector[String] = {
    val viewLine1 = "Der Computerspieler greift das Feld " + coordinates.row + coordinates.col + " an."
    return Vector(viewLine1)
  }

  /** Render info text about ship hit for output
   *
   * @param ship Ship
   * @return Text
   */
  def generateShipHitInfotext(ship: Ship): Vector[String] = {
    val viewLine1 = "Das Schiff " + ship.name + " wurde getroffen."
    return Vector(viewLine1)
  }

  /** Render info text about destroyed ship for output
   *
   * @param ship Ship
   * @return Text
   */
  def generateShipDestroyedInfoText(ship: Ship): Vector[String] = {
    val viewLine1 = "Das Schiff " + ship.name + " wurde versenkt."
    return Vector(viewLine1)
  }
}