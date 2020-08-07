package com.elinger

import org.scalatest.{FlatSpec, Matchers}

class MarbleMazeTest extends FlatSpec with Matchers {

  def findWays(fileName: String)(implicit mazeOps: MarbleMazeOps): Set[(Int, Set[Int])] = {
    val maze = mazeOps.parseInput(fileName)
    mazeOps.findAllInputOutputs(maze)
  }

  "marbles" must "find ways" in {
    val marbleMazeOpsList = List[MarbleMazeOps](MarbleMazeOpsStrategyA, MarbleMazeOpsStrategyFp)
    marbleMazeOpsList.foreach{ mazeOps =>
      implicit val ops: MarbleMazeOps = mazeOps
      findWays("maze1.txt") shouldEqual Set((1, Set(2)))
      findWays("maze5.txt") shouldEqual Set((1, Set(4)), (7, Set(4)))
      findWays("maze7.txt") shouldEqual Set((5, Set(5, 1)), (7, Set(7)), (9, Set(13, 9)))
      findWays("maze8.txt") shouldEqual Set((5, Set(3)))
      findWays("maze9.txt") shouldEqual Set((1, Set(3)))
      findWays("maze10.txt") shouldEqual Set((1, Set()))
      findWays("maze11.txt") shouldEqual Set((5, Set(5)))
      // by https://github.com/SantoJambit/code-kata/tree/marble-maze/mazes
      findWays("the-answer-to-everything.txt") shouldEqual Set(5, 11, 62).map((_, Set.empty)) ++ Set(22, 42, 50, 72, 76, 87).map((_, Set(42)))
      findWays("n-to-n.txt") shouldEqual Set(4, 9, 14, 19).map((_, Set(4, 9, 14, 19)))
    }
  }

}
