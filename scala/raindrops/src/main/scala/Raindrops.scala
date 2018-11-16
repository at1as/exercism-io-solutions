import scala.collection.mutable.ListBuffer

object Raindrops {
  def convert(n: Int): String = {
    val out = new ListBuffer[String]()

    if (n % 3 == 0) out += "Pling"
    if (n % 5 == 0) out += "Plang"
    if (n % 7 == 0) out += "Plong"

    if (out.isEmpty) n.toString else out.mkString("")
  }
}

