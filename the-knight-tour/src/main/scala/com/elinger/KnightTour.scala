package com.elinger

object KnightTour {

  /**
   * Position on the board.
   */
  case class Pos(x: Int, y: Int)

  def createBoard(size: Int): Array[Array[Boolean]] = Array.fill[Boolean](size, size)(false)

  def copy(input: Array[Array[Boolean]]): Array[Array[Boolean]] =
    input.map(_.map(x => x))

  def findTours(sX: Int, sY: Int, size: Int): List[List[Pos]] = {

    def extendTour(pos: Pos, path: List[Pos], board: Array[Array[Boolean]]): Option[List[Pos]] =
      if (pos.x < size && pos.y < size && pos.x >= 0 && pos.y >= 0 && !board(pos.x)(pos.y))
        Some(pos :: path)
      else
        None

    /**
     *
     * @param lastPos last position in the tour
     * @param cTour current tour expressed as a sequence of positions
     * @param board to keep track of used positions so far in the tour
     * @param tourLength so far
     * @return
     */
    def findToursHelper(lastPos: Pos, cTour: List[Pos], board: Array[Array[Boolean]], tourLength: Int): List[List[Pos]] = {
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
      val newTours = nextPositions.flatMap(p => extendTour(p, cTour, board))

      val contTours = newTours.flatMap(tour => {
        val newBoard = copy(board)
        val addedPos  = tour.head
        newBoard(addedPos.x)(addedPos.y) = true
        val newToursLength = tourLength + 1
        if (newToursLength < (size * size))
          findToursHelper(tour.head, tour, newBoard, newToursLength)
        else
          List(tour)
      })

      contTours
    }

    val board = createBoard(size)
    board(sX)(sY) = true
    findToursHelper(Pos(sX, sY), List(Pos(sX, sY)), board, 1)
  }

  def main(args: Array[String]): Unit = {
    val size  = 5
    val start = System.currentTimeMillis()
    println(s"Board size: $size")
    // define different starting position here
    val tours      = findTours(0, 0, size)
    val numOfTours = tours.length
    println(s"Total num of tours: $numOfTours")

    val end = System.currentTimeMillis()
    println(s"Time: ${end - start} ms")


    // let print the moves of one tour
    val aTour = tours.headOption.map(_.reverse)

    val board = Array.tabulate[String](size + 1, size + 1){
      (x, y) =>
        if (x == 0) "_" + y.toString
        else if (y == 0) "_" + x .toString
        else "  "}

    var counter = 1
    aTour.foreach(l => {
      l.foreach(pos => {
        board(pos.x + 1)(pos.y +1) = f"$counter%02d"
        val boardS = board.map(_.mkString("  ")).mkString("\n")
        println(boardS)
        counter = counter + 1
        println("\n\n")
      })
    })
  }

}
