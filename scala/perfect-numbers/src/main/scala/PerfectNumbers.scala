import NumberType.NumberType

object PerfectNumbers {

  def factorSums(n: Int): Option[Int] = n match {
    case x if x < 1 => None
    case _ =>
      val maxFactor = (n / 2) + 1
      val factorSum = 1.until(maxFactor).filter(n % _ == 0).sum
      Some(factorSum)
  } 

  def classify(n: Int): Either[String, NumberType] = factorSums(n) match {
    case None               => Left("Classification is only possible for natural numbers.")
    case Some(x) if x > n   => Right(NumberType.Abundant)
    case Some(x) if x == n  => Right(NumberType.Perfect)
    case Some(x) if x < n   => Right(NumberType.Deficient)
  }
}

object NumberType extends Enumeration {
  type NumberType = Value

  val Perfect = Value("Perfect")
  val Abundant = Value("Abundant")
  val Deficient = Value("Deficient")
}

