package com.elinger

object KnightTour {

  def createMatrix(size: Int): Array[Array[Boolean]] =
    Array.tabulate(size) { _ =>
      Array.tabulate(size) { _ =>
        false
      }
    }

  def copy(input: Array[Array[Boolean]]): Array[Array[Boolean]] =
    input.map(_.map(x => x))

  def findTours(sRow: Int, sCol: Int, size: Int): List[List[(Int, Int)]] = {

    def appendOrStart(tail: List[List[(Int, Int)]], el: (Int, Int), pathLength: Int) : Option[List[List[(Int, Int)]]] = {
      tail match {
        case List() if pathLength == (size * size) =>
          Some(List(List(el)))
        case List() if pathLength < (size * size) => None
        case _ => Some(tail.map(l => el::l))
      }
    }

    def findTours(row: Int, col: Int, matrix: Array[Array[Boolean]], pathLength: Int): List[List[(Int, Int)]] = {
      if (row < size && col < size && row >= 0 && col >= 0 && !matrix(row)(col)) {
        matrix(row)(col) = true
        val el = (row, col)
        val flattenedList = List(
          appendOrStart(findTours(row + 2, col + 1, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row + 2, col - 1, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row - 2, col + 1, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row - 2, col - 1, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row + 1, col + 2, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row + 1, col - 2, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row - 1, col + 2, copy(matrix), pathLength + 1), el, pathLength),
          appendOrStart(findTours(row - 1, col - 2, copy(matrix), pathLength + 1), el, pathLength)
        ).flatten.flatten

        flattenedList
      } else {
        List()
      }
    }

    val allTours = findTours(sRow, sCol, createMatrix(size), 1)
    allTours
      .filter(list => list.length == (size * size))
  }

  def main(args: Array[String]): Unit = {
    val size = 5
    val start = System.currentTimeMillis()
    println(s"Start for size $size")
    val numOfTours = findTours(1, 1, size).length
    println(s"Number of tours $numOfTours")
    val end = System.currentTimeMillis()
    println(s"Time: ${end - start} ms")
  }

}
