package org.wordchain

import java.io.File
import java.util.Comparator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.{Random, Using}
import collection.mutable.{HashMap, MultiMap, Set}

object WordChainAStar {

  /**
    *
    * @param parent
    * @param word
    * @param g distance from start
    * @param h heuristic (distance to end)
    */
  case class Node(parent: Option[Node], word: String, g: Int, h: Int, f: Int)

  case class Tracker(lhs: String, rhs: String)

  def trackers(word: String): Seq[Tracker] =
    (0 until word.length).map(
      i =>
        Tracker(
          word.substring(0, i),
          word.substring(i + 1, word.length)
      )
    )

  object MinComparator extends Comparator[Node] {
    override def compare(x: Node, y: Node): Int = {
      var r = x.f compare y.f
      if (r == 0) {
        // how to break ties, prefer longer paths
        r = -(x.g compare y.g)
      }
      r
    }
  }

  def countDiffChars(w1: String, w2: String): Int = {
   (w1 zip w2).count(t => t._1 != t._2)
  }

  def prepareGraph(s: String, e: String): MultiMap[Tracker, String]= {
    val graph = new HashMap[Tracker, Set[String]] with MultiMap[Tracker, String]
    val handleWord: String => Unit =  w => trackers(w).foreach(graph.addBinding(_, w))
    loadWords(s.length, handleWord)
    handleWord(e)
    graph
  }

  def loadWords(len: Int, handleWord: String => Unit): Unit = {
    val file = new File(getClass.getClassLoader.getResource("words.txt").getPath)
      Using(Source.fromFile(file)) { source =>
        source.getLines()
          .filter(w => w.length == len)
          .foreach(handleWord)
      }
  }

  def shortestPath(start: String, end: String, graph: MultiMap[Tracker, String]): List[String] = {

    def recreatePath(endNode: Node): List[String] = {
      var path = List(endNode.word)

      @tailrec
      def followParent(n: Node): Unit =
        n.parent match {
          case Some(p) =>
            path = p.word :: path
            followParent(p)
          case None =>
        }

      followParent(endNode)

      path
    }

    def nodeNeighbours(graph: mutable.MultiMap[Tracker, String],
                       closedSet: mutable.HashSet[String],
                       parent: Node): Iterable[Node] =
      trackers(parent.word)
        .flatMap(graph.getOrElse(_, List()))
        .filterNot(closedSet.contains)
        .map(word => {
          val g = parent.g + 1
          val h = countDiffChars(word, end)
          Node(Some(parent), word, g, h, g + h)
        })

    def findShortestPath(): List[String] = {
      val minHeap   = new java.util.PriorityQueue[Node](MinComparator)
      val startNode = Node(None, start, 0, 0, 0)
      minHeap.add(startNode)

      val closedSet = mutable.HashSet[String]()
      val openSet   = mutable.HashMap[String, Node]()
      var sPath: List[String] = List()

      var success = false
      var numOfIt = 0
      while (!success && !minHeap.isEmpty) {
        numOfIt += 1
        val node = minHeap.poll()
        //println(node)
        if (node.word == end) {
          // solution, backtrack and build the list
          sPath = recreatePath(node)
          success = true
        } else {
          // remove node from open set and add it to the closed set
          openSet.remove(node.word)
          closedSet.add(node.word)
          // lets first update node's neighbours
          val neighbours = nodeNeighbours(graph, closedSet, node)
          neighbours.foreach(n => {
            val maybeNode = openSet.get(n.word)
            maybeNode match {
              case Some(c) =>
                if (MinComparator.compare(n, c) < 0) {
                  openSet.update(n.word, n)
                  minHeap.remove(c)
                  minHeap.add(n)
                }
              case None =>
                openSet.addOne(n.word, n)
                minHeap.add(n)
            }
          })
        }
      }

      println(s"Num of iterations: $numOfIt")

      sPath
    }

    findShortestPath()
  }

  def measureTime[T](block: => T): (T, Double) = {
    val t0     = System.nanoTime()
    val result = block
    val t1     = System.nanoTime()
    (result, (t1 - t0) / 1000000.0)
  }

  def prettyPrint(s: String, e: String) {
    println(s"Find path between '$s' and '$e'")

    println("Prepare ...")
    val (graph, timeToPrepare) = measureTime(prepareGraph(s, e))
    println(s"Took $timeToPrepare ms")

    val (path, timeToFindPath) = measureTime[List[String]](shortestPath(s, e, graph))
    println(s"Path ${path.length} -> $path")
    println(s"Took $timeToFindPath ms")
    val total = timeToPrepare + timeToFindPath
    println(s"Total $total ms\n\n")
  }

  def main(args: Array[String]): Unit = {
    println("Warm up JVM")
    val r = new Random(7)
    (0 to 10).foreach(i => {
      val s = r.nextString(i)
      val e = r.nextString(i)
      shortestPath(s, e, prepareGraph(s, e))
    })
    println("Warming up completed.")
    println("\n\n")

    prettyPrint("dog", "cat")
    prettyPrint("cat", "dog")
    prettyPrint("peach", "rogue")
    prettyPrint("rogue", "peach")
    prettyPrint("java", "null")
    prettyPrint("null", "java")
    prettyPrint("duck", "ruby")
    prettyPrint("ruby", "duck")
  }

}
