object BracketPush {

  val bracketChars: List[String] = List("(" , ")", "[", "]", "{", "}")
  var pieceStack:   List[String] = List()
  
  def closingParenthesis(char: String): String = char match {
    case ")" => "("
    case "]" => "["
    case "}" => "{"
  }

  def isBalanced(string: String): Either[String, Boolean] = {
    string.split("").filter(bracketChars.contains(_)).foreach( x => x match {
      case "(" | "[" | "{" => 
        pieceStack = pieceStack ++ List(x)
      case ")" | "]" | "}" =>
        val matchingBrace = closingParenthesis(x)
        val lastOnStack = pieceStack.lastOption

        if (matchingBrace == lastOnStack.getOrElse(None)) {
          pieceStack = pieceStack.dropRight(1)
        } else {
          return Left(f"$matchingBrace was not equal to $lastOnStack")
        }
    })

    if (pieceStack.isEmpty) Right(true) else Left(f"There were unclosed parentheses left on stack $pieceStack")
  }

  def isPaired(string: String): Boolean = {
    pieceStack = List()

    isBalanced(string) match {
      case Left(s)  => println(s) ; false
      case Right(b) => b
    }
  }
}

