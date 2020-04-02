package org.wordchain

import java.util.Comparator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.{ Failure, Success, Using }

object WordChainAStar {

  /**
    *
    * @param parent
    * @param word
    * @param g distance from start
    * @param h heuristic (distance to end)
    */
  case class Node(parent: Option[Node], word: String, g: Int, h: Int, f: Int) {

    override def hashCode(): Int = word.hashCode

    override def equals(obj: Any): Boolean =
      obj match {
        case node: Node => node.word == this.word
        case _ => false
      }
  }

  // how to break ties, compare g!
  object MinComparator extends Comparator[Node] {
    override def compare(x: Node, y: Node): Int = {
      var r = x.f compare y.f
      if (r == 0) {
        // x.h compare y.h
        r = -(x.g compare y.g)
      }
      r
    }
  }

  def loadWords(start: String, end: String): List[String] = {
    val len = start.length
    val maybeWords = Using(Source.fromResource("words.txt")) { source =>
      source.getLines
        //.filter(w => w.length == len && w != start && w != end)
        .filter(w => w.length == len)
        .toList
    }

    maybeWords match {
      case Success(words) => words
      case Failure(e) =>
        println(s"Failed to load words ${e.getMessage}")
        List()
    }
  }

  def shortestPath(start: String, end: String, words: List[String]): List[String] = {
    val wordLength = start.length

    def countDiffChars(w1: String, w2: String): Int = {
      var count = 0
      var i     = 0
      while (i < wordLength) {
        if (w1(i) != w2(i)) count = count + 1
        i = i + 1
      }
      count
    }

    val minHeap   = new java.util.PriorityQueue[Node](MinComparator)
    val startNode = Node(None, start, 0, 0, 0)

    // TODO turn into hash set
    val closedSet = mutable.HashSet[String]()
    val openSet   = mutable.HashMap[String, Node]()

    def findShortestPath(): List[String] = {
      var sPath: List[String] = List()

      var success = false
      var numOfIt = 0
      while (!success && !minHeap.isEmpty) {
        numOfIt += 1
        //if (numOfIt % 10 == 0) {
        //  println(s"It: $numOfIt")
        //  minHeap.stream().limit(10).forEach(n => print(s"${n.word} g:${n.g} h:${n.h} f:${n.f}\t"))
        //  println()
        //}
        val node = minHeap.poll()
        //println(node)
        if (node.word == end) {
          // solution, backtrack and build the list
          sPath = List(end)

          @tailrec
          def followParent(n: Node): Unit =
            n.parent match {
              case Some(p) =>
                sPath = p.word :: sPath
                followParent(p)
              case None =>
            }

          followParent(node)
          success = true
        } else {
          // remove node from open set and add it to the closed set
          openSet.remove(node.word)
          closedSet.add(node.word)
          // lets first update node's neighbours
          val neighbours = nodeNeighbours(node)
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

      println(s"Final it: $numOfIt")

      sPath
    }

    def nodeNeighbours(parent: Node): Iterable[Node] =
      openSet.keys
        .filter(countDiffChars(parent.word, _) == 1)
        .map(word => {
          val g = parent.g + 1
          val h = countDiffChars(word, end)
          Node(Some(parent), word, g, h, g + h)
        })

    minHeap.add(startNode)
    openSet.addAll(words.map(w => (w, Node(None, w, Int.MaxValue, Int.MaxValue, Int.MaxValue))))
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

    println("Load words ...")
    val (words, timeLoad) = measureTime[List[String]](loadWords(s, e))
    println(s"${words.length} loaded in $timeLoad ms")

    val (path, timeFindPath) = measureTime[List[String]](shortestPath(s, e, e :: words))
    println(s"Path ${path.length} -> $path")
    println(s"Done in $timeFindPath ms \n")
  }

  def main(args: Array[String]): Unit = {
    //println()
    //(1 to 10).foreach(_ => prettyPrint("peach", "rogue"))
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
