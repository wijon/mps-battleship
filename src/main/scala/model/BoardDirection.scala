package model

object BoardDirection extends Enumeration {
  type BoardDirection = Value
  val North = Value(0)
  val East = Value(1)
  val South = Value(2)
  val West = Value(3)
}