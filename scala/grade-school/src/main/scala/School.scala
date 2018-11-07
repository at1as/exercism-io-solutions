class School {
  type DB = Map[Int, Seq[String]]

  private var datastore: DB = Map[Int, Seq[String]]().withDefaultValue(Seq())

  def add(name: String, g: Int) = { datastore = db + (g -> (db(g) ++ Seq(name))) }

  def db: DB = datastore

  def grade(g: Int): Seq[String] = db(g)

  def sorted: DB = db.mapValues(x => x.sorted).toSeq.sortBy(_._1).toMap
}
