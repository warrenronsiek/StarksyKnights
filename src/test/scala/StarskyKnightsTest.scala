import org.scalatest.FunSuite
import starskyknights.StarskyKnights

class StarskyKnightsTest extends FunSuite {
  val sk = new StarskyKnights(4, 4)

  test("Generates Valid Moves") {
    assert(sk.generateMoveSet((3,3)) === Set((1, 2), (2,1)))
  }

  test("Validates Valid Path") {
    assert(sk.validateMoves(List((3,2), (2, 0), (1, 2), (3,3))))
  }

  test( "Invalidates Invalid Path") {
    assert(sk.validateMoves(List((3, 2), (3, 0), (1, 1), (3, 1))) === false)
  }
}
