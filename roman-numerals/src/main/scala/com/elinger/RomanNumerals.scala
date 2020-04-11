package com.elinger

object RomanNumerals {

  val MapToRoman = Map(
    1000 -> 'M',
    500 -> 'D',
    100 -> 'C',
    50 -> 'L',
    10 -> 'X',
    5 -> 'V',
    1 -> 'I'
  )

  val MapToArabic = Map(
    'M' -> 1000,
    'D' -> 500,
    'C' -> 100,
    'L' -> 50,
    'X' -> 10,
    'V' -> 5,
    'I' -> 1
  )

  def toRomanNumber(dec: Int): String = {

    def helper(number: Int, base: Int): List[Char] =
      if (number > 0) {
        val (whole, rest) = (number / base, number % base)
        val base10        = base / 10

        whole match {
          case w if w < 4 =>
            List.fill[Char](w)(MapToRoman(base)) ::: helper(rest, base10)
          case w if w >= 4 && w < 5 =>
            List(MapToRoman(base), MapToRoman(5 * base)) ::: helper(rest, base10)
          case w if w >= 5 && w < 9 =>
            List(MapToRoman(5 * base)) ::: helper(number - 5 * base, base)
          case _ =>
            List(MapToRoman(base), MapToRoman(10 * base)) ::: helper(rest, base10)
        }
      } else List[Char]()

    helper(dec, 1000).mkString
  }

  def toArabicNumber(romanNumber: String): Int = {
    def value(index: Int, chars: List[Char]): Int =
      // for cases like IV and similar, add to the sum on I encounter and do nothing (0) on V
      if (index >= 1 && MapToArabic(chars(index)) > MapToArabic(chars(index - 1))) {
        0
      } else if (index < chars.size - 1 && MapToArabic(chars(index)) < MapToArabic(chars(index + 1))) {
        MapToArabic(chars(index + 1)) - MapToArabic(chars(index))
      } else {
        MapToArabic(chars(index))
      }

    val chars = romanNumber.toList
    chars.zipWithIndex.map(c => value(c._2, chars)).sum
  }

  def main(args: Array[String]): Unit = {
    println(toRomanNumber(1998))
    println(toArabicNumber("MCMXCVIII"))
  }

}
