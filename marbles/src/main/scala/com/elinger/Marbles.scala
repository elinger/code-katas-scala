package com.elinger

import scala.collection.mutable
import scala.io.Source

trait MazeOps {

  type Grid = Seq[Seq[Boolean]]
  final case class Maze(r: Int, c: Int, grid: Grid)

  def findInputs(maze: Maze): Set[Int] =
    maze.grid.headOption.map(row => findHoles(row.toList)).getOrElse(Set.empty)

  def findOutputs(maze: Maze): Set[Int] =
    maze.grid.reverse.headOption.map(row => findHoles(row.toList)).getOrElse(Set.empty)

  def findHoles(list: List[Boolean]): Set[Int] = list.zipWithIndex.filterNot(t => t._1).map(t => t._2).toSet

  def toString(maze: Maze): String =
    maze.grid.map(_.map(el => if (el) 'X' else ' ').mkString(" ")).mkString("\n")

  def parseInput(fileName: String): Maze = {
    val lines = Source.fromResource(fileName).getLines().toList

    val columns = lines.map(_.length).max
    val rows = lines.length
    val grid = Array.ofDim[Boolean](rows, columns)

    for ((line, row) <- lines.zipWithIndex) {
      line.zipWithIndex.foreach(t => grid(row)(t._2) = t._1 == 'X')
    }

    val immGrid = Seq.tabulate(rows, columns) { (i, j) => grid(i)(j) }

    Maze(rows, columns, immGrid)
  }

  /**
   * @param maze
   * @return input, outputs
   */
  def findAllInputOutputs(maze: Maze): Set[(Int, Set[Int])] =
    findInputs(maze).map(input => (input, findInputOutputs(input, maze)))

  def findInputOutputs(input: Int, maze: Maze): Set[Int]
}


object MazeOpsRecursive extends MazeOps {

  def findInputOutputs(input: Int, maze: Maze): Set[Int] = {
    // cache for situation like this:
    // XX XX XXX
    // XX XX XXX
    // XX      X // common path
    // XXXXXXX X
    val map = mutable.Map[String, Set[Int]]()

    /**
      * @param previous
     *  @param current
      * @return
      */
    def utilFindOutputsForInput(previous: (Int, Int), current: (Int, Int)): Set[Int] = {
      val (rp, cp) = previous
      val (r, c)   = current

      def isInMaze(ri: Int, ci: Int)   = ri < maze.r && ci < maze.c && ci >= 0
      def isFullCell(ri: Int, ci: Int) = maze.grid(ri)(ci)
      val key                          = createKey(r, c)

      if (!isInMaze(r, c) || isFullCell(r, c)) {
        Set.empty[Int]
      } else if (r == maze.r - 1) {
        Set(c)
      } else
        map.getOrElseUpdate(
          key, {
            val canGoDown = isInMaze(r + 1, c) && !isFullCell(r + 1, c)
            val down      = if (canGoDown) utilFindOutputsForInput((r, c), (r + 1, c)) else Set.empty[Int]

            def cameFromRight() = (rp == r) && cp == (c + 1)
            def cameFromLeft()  = (rp == r) && cp == (c - 1)

            val right =
              if (!canGoDown && !cameFromRight())
                utilFindOutputsForInput((r, c), (r, c + 1))
              else Set.empty[Int]

            val left =
              if (!canGoDown && !cameFromLeft())
                utilFindOutputsForInput((r, c), (r, c - 1))
              else Set.empty[Int]

            down union right union left
          }
        )
    }

    // -1 came from top
    utilFindOutputsForInput((0, -1), (0, input))
  }

  private def createKey(r: Int, c: Int) = s"$r-$c"
}

object MarblesApp {
  def main(args: Array[String]): Unit = {
    val mazeOps = MazeOpsRecursive
    val maze = mazeOps.parseInput("maze7.txt")
    println(mazeOps.toString(maze))
    println(mazeOps.findInputs(maze).mkString(" "))
    println(mazeOps.findOutputs(maze).mkString(" "))

    mazeOps.findAllInputOutputs(maze).foreach(println)
  }
}
