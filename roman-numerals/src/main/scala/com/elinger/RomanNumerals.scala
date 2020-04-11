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

  def toRomanNumber(dec: Int): String = {

    def helper(number: Int, base: Int): List[Char] =
      if (number > 0) {
        val (whole, rest) = (number / base, number % base)
        val base10        = base / 10

        whole match {
          case w if w < 4 =>
            List.fill[Char](w)(MapToRoman(base)) ::: helper(rest, base10)
          case w if w >=4 && w < 5 =>
            List(MapToRoman(base), MapToRoman(5 * base)) ::: helper(rest, base10)
          case w if w >=5 && w < 9 =>
            List(MapToRoman(5 * base)) ::: helper(number - 5 * base, base)
          case _ =>
            List(MapToRoman(base), MapToRoman(10 * base)) ::: helper(rest, base10)
        }
      } else List[Char]()

    helper(dec, 1000).mkString
  }


  def main(args: Array[String]): Unit =
    println(toRomanNumber(1998))
}
