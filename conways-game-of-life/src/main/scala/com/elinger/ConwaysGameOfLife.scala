package com.elinger

import com.elinger.ConwaysGameOfLife.Cell.Cell
import scala.annotation.tailrec

object ConwaysGameOfLife {

  object Cell extends Enumeration {
    type Cell = Value
    val Dead, Alive = Value
  }

  case class State(grid: IndexedSeq[IndexedSeq[Cell]], m: Int, n: Int)

  def play(states: List[State], endIteration: Int): List[State] = {

    @tailrec
    def playUtil(states: List[State], iteration: Int, endIteration: Int): List[State] =
      if (iteration == endIteration) states
      else
        playUtil(
          evolve(states.head) :: states,
          iteration + 1,
          endIteration
        )

    playUtil(states, 0, endIteration)
  }

  private def evolve(state: State) = {
    val newGrid =
      IndexedSeq.tabulate(state.m, state.n)((i, j) => {
        val numAliveNeighbours = countAliveNeighbours(state, i, j)
        state.grid(i)(j) match {
          case Cell.Dead if numAliveNeighbours == 3 => Cell.Alive
          case Cell.Alive if numAliveNeighbours == 2 || numAliveNeighbours == 3 => Cell.Alive
          case _ => Cell.Dead
        }
      })
    State(newGrid, state.m, state.n)
  }

  private def getNeighbours(state: State, i: Int, j: Int): Iterator[Cell] = {
    val neighbours = for {
      p <- Seq(i - 1, i, i + 1)
      k <- Seq(j - 1, j, j + 1)
      if !(p == i && k == j)
    } yield getNeighbour(state, p, k)
    // there are some None neighbours, therefore flatMap
    neighbours.flatMap(_.toList).iterator
  }

  private def countAliveNeighbours(state: State, i: Int, j: Int): Int =
    getNeighbours(state, i, j).count(_ == Cell.Alive)

  private def getNeighbour(state: State, p: Int, k: Int): Option[Cell] =
    if (p >= 0 && p < state.m && k >= 0 && k < state.n) Some(state.grid(p)(k)) else None

  def convertToState(stateAsString: String): State = {
    val grid = stateAsString
      .split("\n")
      .map(line => line.map(c => if (c == '*') Cell.Alive else Cell.Dead))
    State(grid, grid.length, grid(0).length)
  }

  def toString(state: State): String =
    state.grid
      .map(_.map(cell => if (cell == Cell.Alive) '*' else '.').mkString(""))
      .mkString("\n")

  def main(args: Array[String]): Unit = {
    val stateAsString =
      """.*...........
        |**.....*.....
        |.*....**.....
        |.............
        |""".stripMargin

    val state = convertToState(stateAsString)

    play(List(state), 10).reverse
      .foreach(state => {
        println()
        println(toString(state))
        println()
        Thread.sleep(2000)
      })
  }
}
