import scala.annotation.tailrec

object SumOfMultiples {

  // Solution 1: Recursive solution
  @tailrec def nextMultiples(multiples: List[Int], limit: Int): List[Int] = {
    val nextInt = multiples(0) + multiples.last

    if (nextInt >= limit) {
      multiples
    } else {
      nextMultiples(multiples ++ List(nextInt), limit)
    }
  }

  def sumRecursiveSolution(factors: Set[Int], limit: Int): Int = {
    factors.filter(_ < limit).flatMap { x => nextMultiples(List(x), limit) }.toSet.sum
  }

  // Solution 2: Improved solution without recursion
  def sum(factors: Set[Int], limit: Int): Int = {
    1.until(limit).filter(x => factors.exists(f => x % f == 0 )).sum
  }
}

