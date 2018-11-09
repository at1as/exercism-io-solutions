import scala.annotation.tailrec

object CollatzConjecture {

  @tailrec def stepCount(n: Int, count: Int, maxIterations: Int): Either[String, Int] = n match {
    case 1 => Right(count)
    case _ => 
      if (count >= maxIterations)
        Left("Failed to find a solution after $maxIterations cycles")
      else
        stepCount(if (n % 2 == 0) n / 2 else 3 * n + 1 , count + 1, maxIterations)
  }

  def steps(initialValue: Int, maxIters: Int = 100000): Option[Int] = stepCount(initialValue, 0, maxIters) match {
    case Left(_)  => None
    case Right(i) => Some(i)
  }

}
