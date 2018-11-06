object Bob {
  def isEmpty(statement: String): Boolean    = statement.trim.isEmpty
  def isSentence(statement: String): Boolean = statement.matches("^.*[a-zA-Z]+.*$") // matches if statement has at least one letter
  def isYelled(statement: String): Boolean   = statement.toUpperCase == statement && isSentence(statement)
  def isQuestion(statement: String): Boolean = statement.trim.endsWith("?")
 
  // Solution 1: Using Case-Statements
  def responseDefault(statement: String): String = statement match {
    case _ if (isEmpty(statement)) =>
      "Fine. Be that way!"
    case _ if (isYelled(statement) && isQuestion(statement)) =>
      "Calm down, I know what I'm doing!"
    case _ if (isYelled(statement)) =>
      "Whoa, chill out!"
    case _ if (isQuestion(statement)) =>
      "Sure."
    case _ =>
      "Whatever."
  }

  // Solution 2: Using Unapply
  case object Empty {
    def unapply(statement: String) = statement.trim.isEmpty
  }
  case object Question {
    def unapply(statement: String) = statement.trim.endsWith("?")
  }
  case object Yelled {
    def unapply(statement: String) = statement.toUpperCase == statement && isSentence(statement)
    def isSentence(statement: String) = statement.matches("^.*[a-zA-Z]+.*$")
  }
  case object YelledQuestion {
    def unapply(statement: String) = Yelled.unapply(statement) && Question.unapply(statement)
  }

  def response(statement: String): String = statement match {
    case Empty()          => "Fine. Be that way!"
    case YelledQuestion() => "Calm down, I know what I'm doing!"
    case Yelled()         => "Whoa, chill out!"
    case Question()       => "Sure."
    case _                => "Whatever."
  }
}
