package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/aspc/
  */

object IntroductionToAlternativeSplicing {

  object SampleData {
    val sample: List[String] = List("6 3")
  }

  import scala.collection.mutable.{Map => MutableMap}
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}

  val inputFileName: String = "/stronghold/datasets/rosalind_aspc.txt"

  private val Modulus: Int = 1000000

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.map(convertStringToIntList(_)).head
  }

  def sumOfSubsetsOfGivenSize(n: Int, m: Int): Int = {
    val pascalTriangle: MutableMap[(Int, Int), Int] = MutableMap()

    def getBinomialCoefficient(a: Int, b: Int): Int = {
      if (b == a || b == 0) 1
      else {
        pascalTriangle.get((a, b)) match {
          case Some(coeff) => coeff
          case None =>
            val coeff: Int = (getBinomialCoefficient(a - 1, b - 1) + getBinomialCoefficient(a - 1, b)) % Modulus
            pascalTriangle += (a,b) -> coeff
            coeff
        }
      }
    }

    (m to n).foldLeft(0)((acc, k) => (acc + getBinomialCoefficient(n, k)) % Modulus)
  }

  def main(args: Array[String]): Unit = {
    val List(n, m): List[Int] = getData(isPractice = false)
    val result: Int = sumOfSubsetsOfGivenSize(n, m)
    println(result)
  }

}
