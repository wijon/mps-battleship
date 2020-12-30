package dataTransferObjects

import scala.util.Try

case class Coordinates(row: Int, col: Int)

object Coordinates {
  /** Extract coordinates from string
   *
   * @param input Coordinates as String in form "[row][col]"
   */
  def coordinatesFromString(input: String): Try[Coordinates] = {
    Try({
      val row = input.slice(0, 1)
      val col = input.slice(1, 2)

      if (!row.charAt(0).isDigit) throw IndexOutOfBoundsException("row")
      if (!col.charAt(0).isDigit) throw IndexOutOfBoundsException("col")

      this (row.toInt, col.toInt)
    })
  }
}