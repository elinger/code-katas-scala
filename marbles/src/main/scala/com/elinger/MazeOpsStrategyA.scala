package com.elinger
import scala.collection.mutable

object MazeOpsStrategyA extends MazeOps {

  def findOutputs(input: Int, maze: Maze): Set[Int] = {
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
