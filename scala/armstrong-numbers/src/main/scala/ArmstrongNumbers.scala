object ArmstrongNumbers {
  def isArmstrongNumber(n: Int): Boolean = {
    val nums = n.toString.split("").map(_.toInt)

    n == nums.fold(0) { (acc, i) => acc + scala.math.pow(i.toInt, nums.size).toInt }
  }
}
