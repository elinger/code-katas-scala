package com.elinger

import org.scalatest.{FlatSpec, Matchers}

class MarblesTest extends FlatSpec with Matchers {

  def findWays(fileName: String)(implicit mazeOps: MazeOps): Set[(Int, Set[Int])] = {
    val maze = mazeOps.parseInput(fileName)
    mazeOps.findAllInputOutputs(maze)
  }

  "marbles" must "find ways" in {
    val mazeOpsList = List[MazeOps](MazeOpsStrategyA, MazeOpsStrategyFp)
    mazeOpsList.foreach{ mazeOps =>
      implicit val ops: MazeOps = mazeOps
      findWays("maze1.txt") shouldEqual Set((1, Set(2)))
      findWays("maze5.txt") shouldEqual Set((1, Set(4)), (7, Set(4)))
      findWays("maze7.txt") shouldEqual Set((5, Set(5, 1)), (7, Set(7)), (9, Set(13, 9)))
      findWays("maze8.txt") shouldEqual Set((5, Set(3)))
      findWays("maze9.txt") shouldEqual Set((1, Set(3)))
      findWays("maze10.txt") shouldEqual Set((1, Set()))
      findWays("maze11.txt") shouldEqual Set((5, Set(5)))
      findWays("maze12.txt") shouldEqual Set((3, Set(10)), (7, Set(10)))
    }
  }

}
