package com.elinger

import scala.io.Source

object IBeforeEExceptAfterC {

  case class Accumulator(ie: Int, cei: Int, ei: Int, cie: Int) {
    def prettyString(): String =
      s"""ie=${this.ie} cie=${this.cie} cei=${this.cei} ei=${this.ei}
      |The first sentence 'ie > 2 * cie' is ${this.ie > 2 * this.cie}
      |The second sentence 'cei > 2 * ei' is ${this.cei > 2 * this.ei}""".stripMargin
  }

  def handle(wordList: List[String]): Accumulator =
    wordList.foldLeft(Accumulator(0, 0, 0, 0)) { (acc, word) =>
      val wordCheck = checkWord(word)
      Accumulator(
        ie  = acc.ie + wordCheck.ie,
        cei = acc.cei + wordCheck.cei,
        ei  = acc.ei + wordCheck.ei,
        cie = acc.cie + wordCheck.cie
      )
    }

  private def checkWord(word: String) = {

    /**
      *
      * @param pp previous of the previous character
      * @param p previous character
      * @param ie 1 if ie case found, otherwise 0
      * @param cei 1 if cei case found, otherwise 0
      * @param ei 1 if ei case found, otherwise 0
      * @param cie 1 if cie case found, otherwise 0
      */
    case class Acc(pp: Option[Char], p: Option[Char], ie: Byte, cei: Byte, ei: Byte, cie: Byte)
    // slide through the word and try to find the substrings
    word.foldLeft(Acc(None, None, ie = 0, cei = 0, ei = 0, cie = 0)) { (a, c) =>
      {
        // format: off
      c.toUpper match {
        case 'E' =>
          a match {
            case Acc(None, Some('I'), _, _, _, _) => a.copy(pp = Some('I'), p = Some('E'), ie = 1)
            case Acc(Some('C'), Some('I'), _, _, _, _) => a.copy(pp = Some('I'), p = Some('E'), cie = 1)
            case Acc(Some(_), Some('I'), _, _, _, _) => a.copy(pp = Some('I'), p = Some('E'), ie = 1)
            case Acc(_, p, _, _, _, _) => a.copy(pp = p, p = Some('E'))
          }
        case 'I' =>
          a match {
            case Acc(None, Some('E'), _, _, _, _) => a.copy(pp = Some('E'), p = Some('I'), ei = 1)
            case Acc(Some('C'), Some('E'), _, _, _, _) => a.copy(pp = Some('E'), p = Some('I'), cei = 1)
            case Acc(Some(_), Some('E'), _, _, _, _)  => a.copy(pp = Some('E'), p = Some('I'), ei = 1)
            case Acc(_, p, _, _, _, _) => a.copy(pp = p, p = Some('I'))
          }
        case c => a.copy(pp = a.p, Some(c))
      }
      // format: on
      }
    }
  }

  def main(arg: Array[String]): Unit = {
    val wordList = Source.fromResource("unixdict.txt").getLines().toList
    val counter  = handle(wordList)
    println(counter.prettyString())
  }

}
