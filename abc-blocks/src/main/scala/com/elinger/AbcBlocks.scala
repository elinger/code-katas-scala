package com.elinger

import java.util.Locale

import scala.io.Source

object AbcBlocks {

  def canMakeWord(word: String, blocks: List[String]): Boolean = {
    val wordUppercase = word.toUpperCase(Locale.ENGLISH).toList
    val blockSet = blocks
      // filter out blocks which do not contain any letter from the word
      .filter(block => wordUppercase.exists(block.contains(_)))
      // since some blocks might be repeated, an index will be added so that a Set can be used for efficient lookups
      .zipWithIndex
      .map(t => s"${t._2}_${t._1}")
      .toSet

    utilCanMakeWord(wordUppercase, blockSet, Set())
  }

  private def utilCanMakeWord(word: List[Char], blocks: Set[String], usedBlocks: Set[String]): Boolean =
    word match {
      case List() => true
      case x :: xs =>
        val blockCandidates = blocks.filter(_.contains(x)).diff(usedBlocks)
        blockCandidates.exists(candidate => utilCanMakeWord(xs, blocks, usedBlocks + candidate))
    }

  def main(args: Array[String]): Unit = {
    val blocks = Source.fromResource("blocks.txt").getLines().toList

    println(canMakeWord("A", blocks))
    println(s""""BARK can be made" is ${canMakeWord("BARK", blocks)}""")
    println(s""""BOOK can be made" is ${canMakeWord("BOOK", blocks)}""")
  }

}
