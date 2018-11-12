object SecretHandshake {

  // Solution 1: solution using recursion
  def nextCommand(cmds: List[String], bin: List[Char]): List[String] = bin match {
    case head :: tail => 
      if (bin(0) == '1') List(cmds.head) ++ nextCommand(cmds.tail, tail) else nextCommand(cmds.tail, tail)
    case Nil =>
      List()
  }

  def commands(n: Int): List[String] = {
    val ops = List("wink", "double blink", "close your eyes", "jump", "reverse")
    val bin = n.toBinaryString.toList.reverse
   
    val output = nextCommand(ops, bin)
    if (output.contains("reverse"))
      output.filterNot(_ == "reverse").reverse
    else
      output
  }

  // Solution 2: functional solution
  def commandsFunctional(n: Int): List[String] = {
    val ops = List("wink", "double blink", "close your eyes", "jump", "reverse")
    val bin = n.toBinaryString.toSeq.reverse

    val output = ops.zip(bin).filter { case(op, b) => b == '1'  }.map(_._1)

    if (output.contains("reverse"))
      output.filterNot(_ == "reverse").reverse
    else
      output
  }

  // Solution 3: Simple solution
  def commandsSimple(n: Int): List[String] = {
    var ops: List[String] = List()
    
    if ((n & 1) != 0) ops = ops :+ "wink"
    if ((n & 2) != 0) ops = ops :+ "double blink"
    if ((n & 4) != 0) ops = ops :+ "close your eyes"
    if ((n & 8) != 0) ops = ops :+ "jump"

    if ((n & 16) != 0)
      ops.reverse
    else
      ops
  }
}
