package utils

object Implicits {

  implicit class ExtendedInteger(n: Int) {
    import ExtendedInteger.calcBinomialCoefficient

    def choose(k: Int): Long = {
      require(k >= 0 && k <= n)
      if (n - k < k) choose(n - k)
      else calcBinomialCoefficient(n, k)
    }

  }

  object ExtendedInteger {
    def calcBinomialCoefficient(n: Int, k: Int): Long = {
      if (k == 0) 1L
      else if (k == 1) n.toLong
      else (n * calcBinomialCoefficient(n - 1, k - 1)) / k
    }
  }

}
