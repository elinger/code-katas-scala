package com.elinger

import org.scalatest.FlatSpec

class RomanNumeralsSpec extends FlatSpec {

  "The conversion" should "should work" in {
    assert(RomanNumerals.toRomanNumber(2020) === "MMXX")
    assert(RomanNumerals.toRomanNumber(8) === "VIII")
    assert(RomanNumerals.toRomanNumber(31) === "XXXI")
    assert(RomanNumerals.toRomanNumber(80) === "LXXX")
    assert(RomanNumerals.toRomanNumber(369) === "CCCLXIX")
    assert(RomanNumerals.toRomanNumber(1998) === "MCMXCVIII")
  }
}
