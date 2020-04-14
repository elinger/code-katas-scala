package com.elinger

object KnightTour {

  /**
   * Position on the board.
   */
  case class Pos(x: Int, y: Int)

  def findTours(sX: Int, sY: Int, size: Int): List[List[Pos]] = {

    def extendTour(pos: Pos, path: List[Pos], takenPositions: Set[Pos]): Option[List[Pos]] =
      if (pos.x < size && pos.y < size && pos.x >= 0 && pos.y >= 0 && !takenPositions.contains(pos))
        Some(pos :: path)
      else
        None

    /**
     *
     * @param lastPos last position in the tour
     * @param cTour current tour expressed as a sequence of positions
     * @param takenPositions to keep track of used positions so far in the tour
     * @param tourLength so far
     * @return
     */
    def findToursHelper(lastPos: Pos, cTour: List[Pos], takenPositions: Set[Pos], tourLength: Int): List[List[Pos]] = {
      val nextPositions = for {
        (i, j) <- List(
          (-1, -2),
          (-1, 2),
          (1, -2),
          (1, 2),
          (-2, 1),
          (-2, -1),
          (2, 1),
          (2, -1)
        )
      } yield Pos(lastPos.x + i, lastPos.y + j)

      // extend and keep only tours which are extended
      val newTours = nextPositions.flatMap(p => extendTour(p, cTour, takenPositions))

      val contTours = newTours.flatMap(tour => {
        val newToursLength = tourLength + 1
        if (tourLength + 1 < (size * size)) {
          val addedPos  = tour.head
          findToursHelper(tour.head, tour, takenPositions + addedPos , newToursLength)
        } else
          List(tour)
      })

      contTours
    }

    val takenPositions = Set[Pos](Pos(sX, sY))
    findToursHelper(Pos(sX, sY), List(Pos(sX, sY)), takenPositions, 1)
  }

  def printMoves(size: Int, tour: List[Pos]): Unit = {
    val board = Array.tabulate[String](size + 1, size + 1){
      (x, y) =>
        if (x == 0) "_" + y.toString
        else if (y == 0) "_" + x .toString
        else "  "}

    var counter = 1
    tour.foreach(pos => {
        println(s"Move: $counter")
        board(pos.x + 1)(pos.y +1) = f"$counter%2d"
        val boardS = board.map(_.mkString("  ")).mkString("\n")
        println(boardS)
        counter = counter + 1
        println("\n\n")
    })
  }

  def main(args: Array[String]): Unit = {
    val size  = 5
    val start = System.currentTimeMillis()
    println(s"Board size: $size")
    // define different starting position here
    val tours      = findTours(0, 2, size)
    val numOfTours = tours.length
    println(s"Total num of tours: $numOfTours")

    val end = System.currentTimeMillis()
    println(s"Time: ${end - start} ms\n\n")

    // let print the moves of one tour
    val maybeTour = tours.headOption.map(_.reverse)
    maybeTour.foreach(tour =>
      printMoves(size, tour))
  }

}
