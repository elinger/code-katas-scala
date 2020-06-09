package com.elinger

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import AbcBlocks.canMakeWord

class AbcBlocksTest extends FlatSpec with Matchers {

  private lazy val blocks = Source.fromResource("blocks.txt").getLines().toList

  "Positive cases" should "work" in {
    canMakeWord("A", blocks)  should be (true)
    canMakeWord("BARK", blocks)  should be (true)
    canMakeWord("TREAT", blocks)  should be (true)
    canMakeWord("SQUAD", blocks)  should be (true)
    canMakeWord("CONFUSE", blocks)  should be (true)
  }

  "Negative cases" should "work" in {
    canMakeWord("BOOK", blocks)  should be (false)
    canMakeWord("COMMON", blocks)  should be (false)
  }
}
