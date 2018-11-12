import scala.annotation.tailrec

object SecretHandshake {

  // Select Solution. Toggle 'solutionNumber' from 1 to 3 to use various implementaitons
  def commands(n: Int): List[String] = {
    def solutionPicker(num: Int): List[String] = num match {
      case 1 => commandsRecursive(n)
      case 2 => commandsFunctional(n)
      case 3 => commandsSimple(n)
    }

    val solutionNumber = 1
    solutionPicker(solutionNumber)
  }


  // Solution 1:
  //    using tail recursion
 @tailrec def nextCommand(cmds: List[String], bin: List[Char], instructions: List[String] = List()): List[String] = bin match {
   case head :: tail =>
      if (bin.head == '1')
        nextCommand(cmds.tail, tail, List(cmds.head) ++ instructions)
      else
        nextCommand(cmds.tail, tail, instructions)
   case Nil =>
     instructions.reverse
 }

  def commandsRecursive(n: Int): List[String] = {
    val ops = List("wink", "double blink", "close your eyes", "jump", "reverse")
    val bin = n.toBinaryString.toList.reverse
   
    val output = nextCommand(ops, bin)
    if (output.contains("reverse"))
      output.filterNot(_ == "reverse").reverse
    else
      output
  }


  // Solution 2: 
  //    functional solution
  def commandsFunctional(n: Int): List[String] = {
    val ops = List("wink", "double blink", "close your eyes", "jump", "reverse")
    val bin = n.toBinaryString.toList.reverse

    val output = ops.zip(bin).filter { case(_op, b) => b == '1' }.map(_._1)

    if (output.contains("reverse"))
      output.filterNot(_ == "reverse").reverse
    else
      output
  }


  // Solution 3: 
  //    Simple solution
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
