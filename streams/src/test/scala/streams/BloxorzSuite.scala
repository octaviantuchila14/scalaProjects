package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """S
        |o
        |o
        |T""".stripMargin

    val optsolution = List(Down, Down)
  }

  trait Level3 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """oS
        |oo
        |oo
        |To""".stripMargin

    val optsolution = List(Down, Left, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("optimal solution for level 2") {
    new Level2 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution for level 3") {
    new Level3 {
      assert(solution.length == optsolution.length)
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("getting right neighbours") {
    new Level1 {
      val res = List(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      val nl = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toList

      assert(nl.size == res.size && nl.forall(n => res.exists(r => r._1 == n._1 && r._2.toSet == n._2.toSet)))
    }
  }

  test("getting new neighbours") {
    new Level1 {
      val res =   Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toList

      val nl = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ).toList

      assert(nl.size == res.size && nl.forall(n => res.exists(r => r._1 == n._1 && r._2.toSet == n._2.toSet)))
    }
  }

}
