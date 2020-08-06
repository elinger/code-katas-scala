package com.elinger

import scala.annotation.tailrec

object MazeOpsStrategyFp extends MazeOps {

  override def findOutputs(input: Int, maze: MazeOpsStrategyFp.Maze): Set[Int] = {

    /**
      *
      * @param inputs in the previous row
      * @param row current row to check
      * @param maze
      * @return
      */
    @tailrec
    def doFindOutputs(inputs: Seq[Int], row: Int, maze: Maze): Seq[Int] = {
      if (row == maze.r)
        inputs
      else {
        val (downOutputs, inputsNoDownOutputs) = inputs.partition(maze.isEmpty(row)(_))

        val rights = findRight(inputsNoDownOutputs, row, maze)
        val lefts = findLeft(inputsNoDownOutputs, row, maze)

        doFindOutputs(
          (downOutputs concat lefts concat rights).distinct.sorted,
          row + 1,
          maze
        )
      }
    }

    if (isEmpty(maze)) Set.empty[Int]
    else if (maze.r == 1) Set[Int](input)
    else doFindOutputs(Seq[Int](input), row = 1, maze).toSet
  }

  def findRight(inputs: Seq[Int], row: Int, maze: Maze): Seq[Int] = {
    // Make pairs and search between. E.g., for the following inputs 3, 8, 10 and maze width 12 we would
    // get the following pairs (3, 8), (8, 10), (10, 12) and search only between starting from left to right.
    // This will guarantee that we spend O(width).
    ((inputs :+ maze.c) zip (inputs :+ maze.c).tail)
      .flatMap{t =>
        // next Input
        val (input, bound) = t
        ((input + 1) until bound).takeWhile(col =>
          maze.isEmpty(row-1)(col)
        ).find(maze.isEmpty(row)(_))
      }
  }

  def findLeft(inputs: Seq[Int], row: Int, maze: Maze): Seq[Int] = {
    // Make pairs and search between. E.g., for the following inputs 3, 8, 10,
    // we get the following pairs (0, 8), (3, 8), (8, 10), (10, 12) and search only between from right to left.
    // This will guarantee that we spend O(width).
    ((Seq(0) ++ inputs) zip (Seq(0) ++ inputs).tail)
      .flatMap{t =>
        // next Input
        val (bound, input) = t
        ((bound + 1) until input).reverse.takeWhile(col =>
          maze.isEmpty(row-1)(col)
        ).find(maze.isEmpty(row)(_))
      }
  }

}
