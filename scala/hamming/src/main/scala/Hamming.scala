object Hamming {

  def distance(str1: String, str2: String): Option[Int] = {
    if (str1.length != str2.length)
      None 
    else
      Some(str1.toSeq.zip(str2.toSeq).filter{ case(x, y) => x != y }.size)
  }
}
