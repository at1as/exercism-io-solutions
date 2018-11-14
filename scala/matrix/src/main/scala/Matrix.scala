case class Matrix(matrixDef: String) {

  private def inputToMatrix: Vector[Vector[Int]] =
    matrixDef.split("\n").map { row => row.split(" ").map(_.toInt).toVector }.toVector

  def row(n: Int): Vector[Int]    = inputToMatrix(n)
  def column(n: Int): Vector[Int] = inputToMatrix.map(x => x(n))
}
