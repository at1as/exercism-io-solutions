object Say {
  def inEnglish(n: Long): Option[String] = n match {
    case 0 =>
      Some("zero")
    case _ if n < 0 =>
      None
    case _ if n > 999999999999L =>
      None
    case _ =>
      val siSuffix    = List("", "thousand", "million", "billion")
      val numSegments = n.toString.reverse.grouped(3).toList.map(_.reverse)
      val words       = numSegments.zip(siSuffix).filterNot { case(x, y) => x == "000" }.map { case(x, y) => f"${prefix(x.toInt)} $y" }.reverse.mkString(" ")

      Some(f"$words".trim.stripSuffix("-"))
  }

  def prefix(n: Int): String = {
    var result  = ""
    var numSegs = n.toString.split("")

    if (numSegs.length == 3) { result += hundreds(numSegs(0).toInt) }
    if (numSegs.length >= 2) {
      result += tens(numSegs(numSegs.length - 2).toInt, numSegs.last.toInt)
      result += ones(numSegs(numSegs.length - 2).toInt, numSegs.last.toInt)
    } else {
      result += ones(0, numSegs.last.toInt)
    } 
    
    result
  }

  def hundreds(n: Int): String = n match {
    case x if ones(0, x).isEmpty => ""
    case _                       => f"${ones(0, n)} hundred "
  }

  def tens(t: Int, ones: Int) = t match {
    case 0 => ""
    case 1 => ones match {
        case 0 => "ten"
        case 1 => "eleven"
        case 2 => "twelve"
        case 3 => "thirteen"
        case 4 => "fourteen"
        case 5 => "fifteen"
        case 6 => "sixteen"
        case 7 => "seventeen"
        case 8 => "eighteen"
        case 9 => "nineteen"
      }
    case 2 => "twenty-"
    case 3 => "thirty-"
    case 4 => "forty-"
    case 5 => "fifty-"
    case 6 => "sixty-"
    case 7 => "seventy-"
    case 8 => "eighty-"
    case 9 => "ninety-"
  }

  def ones(tens: Int, ones: Int) = ones match {
    case _ if (tens == 1) => ""
    case 0 => ""
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
  }
}
