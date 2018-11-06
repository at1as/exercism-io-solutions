object BracketPush {

  // TODO: Work in progress

  def matching(char: String): String = {
    char match {
      case "(" => ")"
      case "[" => "]"
      case "{" => "}"
      case ")" => "("
      case "]" => "["
      case "}" => "{"
      case _   => char
    } 
  }
  def isBalanced(string: String): Boolean = {
    string.split("").foreach( x => x )
  }
}

