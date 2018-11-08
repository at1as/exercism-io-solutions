import scala.annotation.tailrec

class Accumulate {

  // for performance, prepend list and reverse as last step
  @tailrec final def mapper[A, B](f: (A) => B, unmapped: List[A], mapped: List[B] = List()): List[B] = unmapped match {
    case List()        => mapped.reverse
    case first :: rest => mapper(f, rest, List(f(first)) ++ mapped)
  }
  
  // Implementing map method
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = mapper(f, list)
  
  // Trivial solution
  def accumulateTrivial[A, B](f: (A) => B, list : List[A]): List[B] = list.map(f(_))
}
