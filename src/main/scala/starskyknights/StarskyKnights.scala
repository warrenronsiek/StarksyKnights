package starskyknights

import scala.collection.mutable
import scala.math.{max, abs}

class StarskyKnights(nRows: Int, nCols: Int) {

  private def mapFactory = mutable.Map[(Int, Int), Boolean]().withDefaultValue(false)

  private val chebyshevDistance = (a: (Int, Int), b: (Int, Int)) => max(abs(a._1 - b._1), abs(a._2 - b._2))

  def generateMoveSet(tile: (Int, Int), traversed: mutable.Map[(Int, Int), Boolean] = mapFactory): Set[(Int, Int)] = {
    Set(
      (tile._1 - 1, tile._2 + 2), (tile._1 + 1, tile._2 + 2),
      (tile._1 + 2, tile._2 + 1), (tile._1 + 2, tile._2 - 1),
      (tile._1 + 1, tile._2 - 2), (tile._1 - 1, tile._2 - 2),
      (tile._1 - 2, tile._2 - 1), (tile._1 - 2, tile._2 + 1)
    ).filter(t => t._1 >= 0 && t._2 >= 0 && t._1 < nCols && t._2 < nRows && !traversed(t))
  }

  def validateMoves(moves: List[(Int, Int)]): Boolean = {
    val head :: tail = moves
    if (tail == List()) {
      true
    } else if (generateMoveSet(tail.head).contains(head)) {
      validateMoves(tail)
    } else {
      false
    }
  }

  def computePath(start: (Int, Int), end: (Int, Int), heuristic: String = "None"): List[(Int, Int)] = {
    var traversed = mapFactory
    val chosenHeuristic = heuristic match {
      case "Chebyshev" => x: List[(Int, Int)] => x.sortWith(chebyshevDistance(_, end) < chebyshevDistance(_, end))
      case "None" => x: List[(Int, Int)] => x
      case _ => x: List[(Int, Int)] => x
    }

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
        chosenHeuristic(possibleMoves.toList)
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
