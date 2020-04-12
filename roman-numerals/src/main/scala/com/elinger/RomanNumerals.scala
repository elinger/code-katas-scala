package com.elinger

object RomanNumerals {

  val Romans = Map(
    1000 -> 'M',
    500 -> 'D',
    100 -> 'C',
    50 -> 'L',
    10 -> 'X',
    5 -> 'V',
    1 -> 'I'
  )

  val Arabics = Map(
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
            List.fill[Char](w)(Romans(base)) ::: helper(rest, base10)
          case w if w >= 4 && w < 5 =>
            List(Romans(base), Romans(5 * base)) ::: helper(rest, base10)
          case w if w >= 5 && w < 9 =>
            List(Romans(5 * base)) ::: helper(number - 5 * base, base)
          case _ =>
            List(Romans(base), Romans(10 * base)) ::: helper(rest, base10)
        }
      } else List[Char]()

    helper(dec, 1000).mkString
  }

  def toArabicNumber(romanNumber: String): Int = {
    def toArabicHelper(number: List[Char]): Int =
      number match {
        case List() => 0
        case List(el) => Arabics(el)
        case first :: second :: rest if Arabics(first) < Arabics(second) =>
          - Arabics(first) + toArabicHelper(second :: rest) // subtract the value (e.g. if XC = (first, second) => -10)
        case first :: second :: rest if Arabics(first) >= Arabics(second) =>
          Arabics(first) + toArabicHelper(second :: rest)
    }
    toArabicHelper(romanNumber.toList)
  }

  def main(args: Array[String]): Unit = {
    println(toRomanNumber(1998))
    println(toArabicNumber("MCMXCVIII"))
  }

}
