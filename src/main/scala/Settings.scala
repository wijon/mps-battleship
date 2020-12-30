import dataTransferObjects.Ship

/** Get all ships for one player
 *
 * @return List of all ships
 */
def getShipsForOnePlayer: Vector[Ship] = 
  Vector(
    Ship(5, "Carrier"),
    Ship(4, "Battleship"),
    Ship(3, "Cruiser"),
    Ship(3, "Submarine"),
    Ship(2, "Destroyer")
  )