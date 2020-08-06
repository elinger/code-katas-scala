package com.elinger

import scala.io.Source

trait MazeOps {

  type Grid = Seq[Seq[Boolean]]
  final case class Maze(r: Int, c: Int, grid: Grid) {
    def isEmpty(ri: Int)(ci: Int): Boolean = !grid(ri)(ci)
  }

  def findInputs(maze: Maze): Set[Int] =
    maze.grid.headOption.map(row => findHoles(row.toList)).getOrElse(Set.empty)

  def findOutputs(maze: Maze): Set[Int] =
    maze.grid.reverse.headOption.map(row => findHoles(row.toList)).getOrElse(Set.empty)

  def findHoles(list: List[Boolean]): Set[Int] = list.zipWithIndex.filterNot(t => t._1).map(t => t._2).toSet

  def isEmpty(maze: Maze): Boolean = maze.r == 0 || maze.c == 0

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
    findInputs(maze).map(input => (input, findOutputs(input, maze)))

  def findOutputs(input: Int, maze: Maze): Set[Int]
}

object MarblesApp {
  def main(args: Array[String]): Unit = {
    val mazeOps = MazeOpsStrategyA
    val maze = mazeOps.parseInput("maze7.txt")
    println(mazeOps.toString(maze))
    println(mazeOps.findInputs(maze).mkString(" "))
    println(mazeOps.findOutputs(maze).mkString(" "))

    mazeOps.findAllInputOutputs(maze).foreach(println)
  }
}
