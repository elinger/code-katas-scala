package com.elinger

object RomanNumerals {

  def toRomanNumber(dec: Int): String = {
    require(dec > 0 && dec <= 3000, "Dec must be between 1 and 3000")
    val (m, mr) = (dec / 1000, dec % 1000)
    var res     = List.fill[Char](m)('M')

    if (mr > 0) {
      val (d, dr) = (mr / 500, mr % 500)
      if (d == 1) {
        if (dr < 400) {
          res = res ::: List('D')
          // calculation for C, L, X, V, I
          res = res ::: convertValueLessThan400(dr)
        } else {
          res = res ::: List('C', 'M')
          // use L, X, V, I to handle value smaller than 100
          res = res ::: convertValueLessOrEqual100(dr - 400)
        }
      } else {
        if (dr < 400) {
          res = res ::: convertValueLessThan400(dr)
        } else {
          res = res ::: List('C', 'D')
          res = res ::: convertValueLessOrEqual100(dr - 400)
        }
      }
    }
    res.mkString
  }

  def convertValueLessThan400(dec: Int): List[Char] = {
    val (c, cr) = (dec / 100, dec % 100)
    var res     = List.fill[Char](c)('C')
    res = res ::: convertValueLessOrEqual100(cr)
    res
  }

  def convertValueLessOrEqual100(dec: Int): List[Char] = {
    var res = List[Char]()

    if (dec < 100) {
      val (l, lr) = (dec / 50, dec % 50)

      if (l == 1) {
        if (lr < 40) {
          res = res ::: List('L')
          res = res ::: convertValueLessThan40(lr)
        } else {
          res = res ::: List('X', 'C')
          res = res ::: convertValueLessOrEqualTo10(lr - 40)
        }
      } else {
        if (lr < 40) {
          res = res ::: convertValueLessThan40(lr)
        } else {
          res = res ::: List('X', 'L')
          res = res ::: convertValueLessOrEqualTo10(lr - 40)
        }
      }
    }

    res
  }

  def convertValueLessThan40(dec: Int): List[Char] = {
    val (x, xr) = (dec / 10, dec % 10)
    var res     = List.fill[Char](x)('X')
    res = res ::: convertValueLessOrEqualTo10(xr)
    res
  }

  def convertValueLessOrEqualTo10(dec: Int): List[Char] = {
    var res = List[Char]()
    if (dec < 10) {
     val (v, vr) = (dec / 5, dec % 5)
      if (v == 1) {
        if (vr == 0) {
          res = res ::: List('V')
        } else if (vr < 4) {
          res = res ::: List('V')
          res = res ::: List.fill(vr)('I')
        } else {
          res = res ::: List('I', 'X')
        }
      } else if (vr < 4) {
       res = res ::: List.fill[Char](vr)('I')
      } else {
        res = res ::: List('I', 'V')
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    println(toRomanNumber(1998))
  }
}
