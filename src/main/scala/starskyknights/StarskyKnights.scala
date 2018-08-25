package starskyknights

import scala.collection.mutable

class StarskyKnights(nRows: Int, nCols: Int) {

  def generateMoveSet(tile: (Int, Int), traversed: mutable.Map[(Int, Int), Boolean] = mutable.Map[(Int, Int), Boolean]().withDefaultValue(false)): Set[(Int, Int)] = {
    Set(
      (tile._1 - 1, tile._2 + 2), (tile._1 + 1, tile._2 + 2),
      (tile._1 + 2, tile._2 + 1), (tile._1 + 2, tile._2 - 1),
      (tile._1 + 1, tile._2 - 2), (tile._1 - 1, tile._2 - 2),
      (tile._1 - 2, tile._2 - 1), (tile._1 - 2, tile._2 + 1)
    ).filter(t => t._1 >= 0 && t._2 >= 0 && t._1 < nCols && t._2 < nRows && !traversed(t))
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

  def computePath(start: (Int, Int), end: (Int, Int)): List[(Int, Int)] = {
    var traversed = mutable.Map[(Int, Int), Boolean]().withDefaultValue(false)

    def recursiveTraversal(path: List[(Int, Int)]): List[(Int, Int)] = {
      val currentPosition = path.head
      if (currentPosition == end) {
        return path
      }
      traversed += (currentPosition -> true)
      val possibleMoves = generateMoveSet(path.head, traversed)
      if (possibleMoves == Set()) {
        path
      } else {
        possibleMoves
          .map(t => recursiveTraversal(t :: path))
          .reduce[List[(Int, Int)]] {
          case l if l._1.head == end && l._2.head == end => if (l._1.length > l._2.length) l._2 else l._1
          case l if l._1.head == end && l._2.head != end => l._1
          case l if l._2.head == end => l._2
          case l => l._1
        }
      }
    }

    recursiveTraversal(List(start))
  }
}
