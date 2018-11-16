object Etl {

  def transform(m: Map[Int, Seq[String]]): Map[String, Int] =
    m.flatMap { case(k, v) =>
      v.map(x => (x.toLowerCase, k))
    }

}

