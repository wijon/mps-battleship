package enums

object BoardDirection extends Enumeration {
  type BoardDirection = Value
  val North: enums.BoardDirection.Value = Value(0)
  val East: enums.BoardDirection.Value = Value(1)
  val South: enums.BoardDirection.Value = Value(2)
  val West: enums.BoardDirection.Value = Value(3)
}
