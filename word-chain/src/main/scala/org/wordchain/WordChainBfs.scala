package org.wordchain

import scala.collection.mutable
import scala.collection.mutable.{ Map => MMap }
import scala.io.Source
import scala.util.{ Failure, Success, Using }

object WordChainBfs {

  def loadWords(start: String, end: String): List[String] = {
    val len = start.length
    val maybeWords = Using(Source.fromResource("words.txt")) { source =>
      source.getLines
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

  def calcShortestPath(start: String, end: String, words: List[String]): (Int, List[String]) = {
    require(
      start.length == end.length,
      "Words must have same length"
    )
    val wordLength   = start.length
    val foundPaths   = calcPaths(start, end, words)
    val shortestPath = foundPaths.map(l => (l.length, l)).minBy(_._1)
    shortestPath
  }

  def calcPaths(start: String, end: String, candidates: List[String]): List[List[String]] = {

    // mutable!
    val neighboursMap = MMap[String, List[String]]()
    // mutable!
    val seenWords  = mutable.HashSet[String]()
    val wordLength = start.length

    def isDiffOne(w1: String, w2: String): Boolean = {
      var count = 0
      var i     = 0
      while (i < wordLength && count < 2) {
        if (w1(i) != w2(i)) count = count + 1
        i = i + 1
      }

      if (count == 1) true else false
    }

    def isValid(word: String, path: List[String]): Boolean =
      path match {
        case head :: rest => isDiffOne(head, word)
        case _ => false
      }

    // discover neighbours lazily on first access
    def getNeighbours(word: String): List[String] =
      neighboursMap.get(word) match {
        case Some(v) => v
        case None =>
          val newVal = candidates.filter(c => isDiffOne(word, c))
          neighboursMap += (word -> newVal)
          newVal
      }

    def calcNewPaths(cPaths: List[List[String]]): List[List[String]] =
      for {
        path <- cPaths
        candidate <- getNeighbours(path.head)
        if isValid(candidate, path) && !seenWords.contains(candidate)
      } yield {
        seenWords += candidate
        candidate :: path
      }

    import scala.annotation.tailrec
    @tailrec
    def paths(cPaths: List[List[String]], it: Int): List[List[String]] = {
      if (it % 10 == 0)
        println(s"It $it")
      // valid paths
      val vPaths = for { path <- cPaths if isValid(start, path) } yield start :: path

      vPaths match {
        case List() => // no valid paths found, keep searching
          // new paths
          val nPaths = calcNewPaths(cPaths)
          nPaths match {
            case List() => List(List())
            case newPaths: List[List[String]] =>
              paths(newPaths, it + 1)
          }
        case validPaths: List[List[String]] => validPaths
      }
    }

    // start from the end and apply breadth first search
    val initPaths: List[List[String]] = List(List(end))
    seenWords += start
    seenWords += end
    paths(initPaths, 1)
  }

  def measureTime[T](block: => T): (T, Long) = {
    val t0     = System.nanoTime()
    val result = block
    val t1     = System.nanoTime()
    (result, (t1 - t0) / 1000000)
  }

  def prettyPrint(s: String, e: String) {
    println(s"Find path between '$s' and '$e'")

    println("Load words ...")
    val (words, timeLoad) = measureTime[List[String]](loadWords(s, e))
    println(s"${words.length} loaded in $timeLoad ms")

    val (shortestPath, timeFindPath) = measureTime[(Int, List[String])](calcShortestPath(s, e, words))
    println(s"Path $shortestPath")
    println(s"Done in $timeFindPath ms \n")
  }

  def main(args: Array[String]): Unit = {
    println()
    prettyPrint("rogue", "peach")
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
