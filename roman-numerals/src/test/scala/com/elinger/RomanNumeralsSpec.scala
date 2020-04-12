package com.elinger

import org.scalatest.FlatSpec

class RomanNumeralsSpec extends FlatSpec {

  "The conversion from arabic to roman" should "work" in {
    assert(RomanNumerals.toRomanNumber(2020) === "MMXX")
    assert(RomanNumerals.toRomanNumber(2798) === "MMDCCXCVIII")
    assert(RomanNumerals.toRomanNumber(8) === "VIII")
    assert(RomanNumerals.toRomanNumber(5) === "V")
    assert(RomanNumerals.toRomanNumber(3) === "III")
    assert(RomanNumerals.toRomanNumber(9) === "IX")
    assert(RomanNumerals.toRomanNumber(31) === "XXXI")
    assert(RomanNumerals.toRomanNumber(80) === "LXXX")
    assert(RomanNumerals.toRomanNumber(369) === "CCCLXIX")
    assert(RomanNumerals.toRomanNumber(351) === "CCCLI")
    assert(RomanNumerals.toRomanNumber(1998) === "MCMXCVIII")
  }

  "The conversion from roman to arabic" should "work" in {
    assert(RomanNumerals.toArabicNumber("MMXX") === 2020)
    assert(RomanNumerals.toArabicNumber("MMDCCXCVIII") === 2798)
    assert(RomanNumerals.toArabicNumber("VIII") === 8)
    assert(RomanNumerals.toArabicNumber("V") === 5)
    assert(RomanNumerals.toArabicNumber("III") === 3)
    assert(RomanNumerals.toArabicNumber("IX") === 9)
    assert(RomanNumerals.toArabicNumber("XXXI") === 31)
    assert(RomanNumerals.toArabicNumber("LXXX") === 80)
    assert(RomanNumerals.toArabicNumber("CCCLXIX") === 369)
    assert(RomanNumerals.toArabicNumber("CCCLI") === 351)
    assert(RomanNumerals.toArabicNumber("MCMXCVIII") === 1998)
  }
}
