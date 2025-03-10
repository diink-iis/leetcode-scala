package medium

object Solution {
  def myAtoi(s: String): Int = {
    s.foldLeft(("", false)) { case ((initStr, stop), inputStr) =>
      if (stop) (initStr, stop) // If a non-digit has been encountered, stop processing
      else inputStr match {
        case ' ' if initStr.isEmpty => (initStr, stop) // Ignore leading spaces
        case '0' if initStr.isEmpty => (initStr, stop) // Ignore leading zeros
        case c if c.isDigit => (initStr + c, stop) // Append digits
        case _ => (initStr, true) // Stop on first non-digit
      }
    }._1 match {
      case "" => 0
      case str => str.toInt
    }
  }
}
