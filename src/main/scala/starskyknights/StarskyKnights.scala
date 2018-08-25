package starskyknights



class StarskyKnights(nRows: Int, nCols: Int) {

  def generateMoveSet(tile: (Int, Int)): Set[(Int, Int)] = {
    Set(
      (tile._1 - 1, tile._2 + 2), (tile._1 + 1, tile._2 + 2),
      (tile._1 + 2, tile._2 + 1), (tile._1 + 2, tile._2 - 1),
      (tile._1 + 1, tile._2 - 2), (tile._1 - 1, tile._2 - 2),
      (tile._1 - 2, tile._2 - 1), (tile._1 - 2, tile._2 + 1)
    ).filter(t => t._1 >= 0 && t._2 >= 0 && t._1 < nCols && t._2 < nRows)
  }

  def validateMoves(moves: List[(Int, Int)]): Boolean = {
    moves match {
      case head :: tail =>
        if (tail == List()) {
          true
        } else if (generateMoveSet(tail.head).contains(head)) {
          validateMoves(tail)
        } else {
          false
        }
      case Nil => true
    }
  }
}
