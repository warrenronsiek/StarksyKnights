import org.scalatest.{FunSpec, FunSuite}
import starskyknights.StarskyKnights

class StarskyKnightsTest extends FunSpec {

  describe("Basic Functionality") {
    val sk = new StarskyKnights(4, 4)
    it("Generates Valid Moves") {
      assert(sk.generateMoveSet((3, 3)) === Set((1, 2), (2, 1)))
    }

    it("Validates Valid Path") {
      assert(sk.validateMoves(List((3, 2), (2, 0), (1, 2), (3, 3))))
    }

    it("Invalidates Invalid Path") {
      assert(sk.validateMoves(List((3, 2), (3, 0), (1, 1), (3, 1))) === false)
    }
  }

  describe("Path Finding Functionality") {
    val sk4 = new StarskyKnights(4, 4)
    val sk12 = new StarskyKnights(12, 12)
    val sk32 = new StarskyKnights(32, 32)

    it("Solves Trivial Path") {
      val path = sk4.computePath((0, 0), (2, 1))
      assert(sk4.validateMoves(path))
    }

    it("Solves Slightly Less Trivial Path") {
      val path = sk12.computePath((1, 7), (2, 10))
      assert(sk12.validateMoves(path))
    }

    it("Solves Non-Trivial Path") {
      val path = sk32.computePath((10, 5), (20, 31))
      assert(sk32.validateMoves(path))
    }

    it("Solves Non-Trivial Path with Bounding") {
      val path = sk32.computePath((10, 5), (20, 31), useBounding = true)
      assert(sk32.validateMoves(path))
    }

    it("Solves Non-Trivial Path with Heuristics and Bounding") {
      val path = sk32.computePath((10, 5), (20, 31), "Chess", useBounding = true)
      assert(sk32.validateMoves(path))
    }

  }

}
