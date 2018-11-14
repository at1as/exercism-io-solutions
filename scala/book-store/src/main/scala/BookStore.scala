import scala.annotation.tailrec

object BookStore {
  private val bookPrice = 800

  def discount(count: Int) = count match {
    case 2 => 0.95
    case 3 => 0.90
    case 4 => 0.80
    case 5 => 0.75
    case _ => 1.00
  }

  @tailrec def bookCombos(books: Map[Int, Int], price: Double = 0.00): Double = books.filter(_._2 > 0).size match {
    case 0 => price
    case _ =>
      val uniqueBooks = books.filter { case(k, v) => v >= 1  }.size
      val cost        = bookPrice * uniqueBooks * discount(uniqueBooks)

      val remainingBooks = books.toList.sortBy(_._2).zipWithIndex.map { case((x, y), c) => if (c < uniqueBooks) (x, y-1) else (x, y) }.toMap

      bookCombos(remainingBooks, price + cost)
  }

  def total(books: List[Int]): Int = {
    val totalBooks  = books.size
    val uniqueBooks = books.toSet.size
    val bookCounts  = books.groupBy(identity).mapValues(_.size)

    bookCombos(bookCounts).toInt
  }

}
