package com.elinger

import scala.io.Source

object IBeforeEExceptAfterC {

  case class Word(w: String, frequency: Int)

  case class Counter(ie: Int, cei: Int, ei: Int, cie: Int) {

    def *(constant: Int): Counter =
      Counter(constant * ie, constant * cei, constant * ei, constant * cie)

    def +(that: Counter): Counter =
      Counter(ie + that.ie, cei + that.cei, ei + that.ei, cie + that.cie)

    def prettyString(): String =
      s"""ie=${this.ie} cie=${this.cie} cei=${this.cei} ei=${this.ei}
         |The first sentence 'ie > 2 * cie' is ${this.ie > 2 * this.cie}
         |The second sentence 'cei > 2 * ei' is ${this.cei > 2 * this.ei}""".stripMargin
  }

  def inspect(wordList: List[Word]): Counter = {

    def checkIfFirstOtherwiseCheckIfSecond(w: String, s1: String, s2: String) =
      if (w.contains(s1))
        (true, false)
      else if (w.contains(s2))
        (false, true)
      else
        (false, false)

    wordList.foldLeft(Counter(0, 0, 0, 0)) { (acc, word) =>
      val w     = word.w

      val (isCei, isEi) = checkIfFirstOtherwiseCheckIfSecond(w, "cei", "ei")
      val (isCie, isIe) = checkIfFirstOtherwiseCheckIfSecond(w, "cie", "ie")

      implicit def boolToInt(b: Boolean): Int = if (b) 1 else 0
      val counter = Counter(
        ie  = isIe,
        cei = isCei,
        ei  = isEi,
        cie = isCie
      )
      acc + (counter * word.frequency)
    }
  }

  def main(arg: Array[String]): Unit = {

    val wordList = Source
      .fromResource("unixdict.txt")
      .getLines()
      .map(Word(_, 1))
      .toList

    time {
      val counter1 = inspect(wordList)
      println(counter1.prettyString())
    }

    println()
    time {
      val wordWithFrequenciesList = Source
        .fromResource("words-with-frequencies.txt")
        .getLines()
        .drop(1)
        .map(_.trim().split("\t"))
        .map(ls => Word(ls(0), ls(2).toInt))
        .toList
      val counter2 = inspect(wordWithFrequenciesList)
      println(counter2.prettyString())
    }
  }

  def time(codeBlock: => Unit): Unit = {
    println("Time tracking started")
    val start = System.currentTimeMillis()
    codeBlock
    val totalTime = System.currentTimeMillis() - start
    println(s"Total time: $totalTime ms")
  }

}
